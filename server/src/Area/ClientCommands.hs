
module Area.ClientCommands where

import qualified Data.Map.Strict as M

import Data.Aeson(Value)
import Data.String.Utils (startswith)
import Data.Lens.Common ((^%=), (^-=))
import Control.Distributed.Process

import Utils (milliseconds)
import Connection (Connection)
import Types (UserId)
import qualified Settings as S
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, angle)
import Area.Types
import Area.State
import Area.Event (Event(Disappearance, Shot), DReason(Exit))
import Area.External (enter)


type Response a = (a, State)

handleEcho :: State -> (Echo, Connection) -> Process (Response String)
handleEcho state (Echo txt, _) = do
    let resp = areaId state ++ " echo: " ++ txt
    return (resp, state)


handleEnterArea :: State -> (EnterArea, Connection) -> Process State
handleEnterArea state (EnterArea aid, conn) = do
    let user@U.User{U.userId=uid} = conn `userByConn` state
        userPid = (userPids . users) state M.! uid
        delUser = users' ^%= deleteUser uid
        addEvent = events' ^%= (Disappearance (UId uid) Exit :)
    enter aid (U.userArea user) userPid False conn
    return $ addEvent $ delUser state


handleObjectsInfo :: State -> (GetObjectsInfo, Connection)
                     -> Process (Response [Value])
handleObjectsInfo state (GetObjectsInfo ids, _) = do
    let objects = foldl getObj [] ids
        us = usersData $ users state
        getObj :: [Value] -> String -> [Value]
        getObj res ident
            | "user-id:" `startswith` ident =
                let ident' = read ident :: UserId
                in case ident' `M.lookup` us of
                    Nothing -> res
                    Just user -> U.initClientInfo user:res
    return (objects, state)


handleMoveTo :: State -> (MoveTo, Connection) -> Process State
handleMoveTo state (MoveTo toPos, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        user = userByConn conn state
        dt = distance (U.pos user) toPos / (fromIntegral (U.speed user) / 1000)
        action = MoveDistance{startTs=now,
                              endTs=now + round dt,
                              from=U.pos user,
                              to=toPos}
        replace MoveDistance{} = True
        replace _ = False
        updActions = replaceUserAction uid replace action
        updUser = updateUser (\u -> u{U.angle=angle (U.pos u) toPos}) uid
    return $ updActions $ updUser state


handleIgnite :: State -> (Ignite, Connection) -> Process State
handleIgnite state (Ignite dmgSpeed, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        action = Burning{previousTs=now,
                         damageSpeed=dmgSpeed}
    return $ addUserAction uid action state


handleShoot :: State -> (Shoot, Connection) -> Process State
handleShoot state (Shoot target, conn) = return $ addEvent $ updUsr state
    where event = Shot (uidByConn conn state) target
          damage = S.shotDamage $ settings state
          addEvent = events' ^%= (event :)
          updUsr = updateUser (U.durability' ^-= damage) target


userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connToIds . users) state M.! conn


updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f = updateUser (U.actions' ^%= f)

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action:) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f

