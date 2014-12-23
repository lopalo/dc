
module Area.ClientCommands where

import qualified Data.Map.Strict as M

import Data.Aeson(Value)
import Data.String.Utils (startswith)
import Data.Lens.Common ((^%=))
import Control.Distributed.Process

import Utils (milliseconds)
import Connection (Connection)
import Types (UserId)
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, angle)
import Area.Types
import Area.State
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
    enter aid (U.userArea user) userPid conn
    return $ (users' ^%= deleteUser uid) state


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


userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connToIds . users) state M.! conn


updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f = updateUser update
    where update user = user{U.actions=f $ U.actions user}

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action:) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f

