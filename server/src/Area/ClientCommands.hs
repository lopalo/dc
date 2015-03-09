
module Area.ClientCommands where

import qualified Data.Map.Strict as M

import Data.Aeson(ToJSON, Value, toJSON)
import Data.String.Utils (startswith)
import Data.Lens.Strict ((^%=), (^-=))
import Control.Distributed.Process

import Utils (milliseconds)
import Connection (Connection, sendResponse)
import Types (UserId, RequestNumber)
import qualified Settings as S
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, angle)
import Area.Types
import Area.State
import Area.Event (Event(Disappearance, Shot), DReason(Exit))
import Area.External (enter)


type Response = Process (Value, State)

response :: ToJSON a => a -> State -> Response
response resp state = return (toJSON resp, state)

handleClientReq :: State -> ((ClientCommand, Connection), RequestNumber) ->
                   Process State
handleClientReq state ((a, conn), req) = do
    (resp, state') <- handleClientRequest state (a, conn)
    sendResponse conn req resp
    return state'


handleClientRequest :: State -> (ClientCommand, Connection) -> Response
handleClientRequest state (Echo txt, _) = do
    let resp = areaId state ++ " echo: " ++ txt
    response resp state

handleClientRequest state (GetObjectsInfo ids, _) = do
    let objects = foldl getObj [] ids
        us = usersData $ users state
        getObj :: [Value] -> String -> [Value]
        getObj res ident
            | "user-id:" `startswith` ident =
                let ident' = read ident :: UserId
                in case ident' `M.lookup` us of
                    Nothing -> res
                    Just user -> U.initClientInfo user:res
    response objects state


handleClientCommand :: State -> (ClientCommand, Connection) -> Process State
handleClientCommand state (EnterArea aid, conn) = do
    let user@U.User{U.userId=uid} = conn `userByConn` state
        userPid = (userPids . users) state M.! uid
        delUser = usersL ^%= deleteUser uid
        addEvent = eventsL ^%= (Disappearance (UId uid) Exit :)
    enter aid (U.userArea user) userPid False conn
    return $ addEvent $ delUser state

handleClientCommand state (MoveTo toPos, conn) = do
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

handleClientCommand state (Ignite dmgSpeed, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        action = Burning{previousTs=now,
                         damageSpeed=dmgSpeed}
    return $ addUserAction uid action state

handleClientCommand state (Shoot target, conn) =
    return $ addEvent $ updUsr state
    where event = Shot (uidByConn conn state) target
          damage = S.shotDamage $ settings state
          addEvent = eventsL ^%= (event :)
          updUsr = updateUser (U.durabilityL ^-= damage) target


userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connToIds . users) state M.! conn


updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f = updateUser (U.actionsL ^%= f)

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action:) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f

