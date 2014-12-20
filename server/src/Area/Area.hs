{-# LANGUAGE OverloadedStrings #-}

module Area.Area (areaProcess) where

import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.), (>>>))
import qualified Data.Map.Strict as M

import Data.Aeson(ToJSON, Value, object, (.=))
import Data.String.Utils (startswith)
import Data.Lens.Common ((^$), (^%=))
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea, sendResponse)
import Utils (milliseconds, logException, evaluate)
import qualified Settings as S
import Types (UserId, UserPid(..), AreaId, AreaPid(..), RequestNumber)
import qualified User.External as UE
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, angle, sendCmd)
import Area.Types
import Area.State
import Area.Tick (handleTick, scheduleTick)
import Area.External (enter)


type Response a = (a, State)


handleEnter :: State -> (Enter, Connection) -> Process State
handleEnter state (Enter ua userPid, conn) = do
    let uid = UE.userId ua
        enterPos = (S.enterPos . settings) state
        user = U.User{U.userId=uid,
                      U.name=UE.name ua,
                      U.pos=uncurry Pos enterPos,
                      U.angle=0,
                      U.speed=UE.speed ua,
                      U.durability=UE.durability ua,
                      U.actions=[]}
        state' = (users' ^%= addUser uid conn userPid user) state
    UE.monitorUser userPid
    initConnection conn state'
    UE.syncState userPid $ U.userArea user $ areaId state'
    return state'

handleReconnection :: State -> (Reconnection, Connection) -> Process State
handleReconnection state (Reconnection uid, conn) = do
    let connections'' = connections' . users'
        connToIds'' = connToIds' . users'
        oldConn = (connections'' ^$ state) M.! uid
        changeConnection = (connections'' ^%= M.insert uid conn) >>>
                           (connToIds'' ^%= M.delete oldConn) >>>
                           (connToIds'' ^%= M.insert conn uid)
        state' = changeConnection state
    evaluate state'
    initConnection conn state'
    return state'

handleMonitorNotification :: State -> ProcessMonitorNotification
                             -> Process State
handleMonitorNotification state (ProcessMonitorNotification ref pid _) = do
    unmonitor ref
    case UserPid pid `M.lookup` userPidToIds (users state) of
        Just uid -> return $ (users' ^%= deleteUser uid) state
        Nothing -> return state

--TODO: move client commands to the separate module
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


areaProcess :: S.AreaSettings -> AreaId -> Process ()
areaProcess aSettings aid = do
    let state = State{areaId=aid,
                      settings=aSettings,
                      tickNumber=0,
                      users=us,
                      events=[],
                      eventsForBroadcast=[]}
        us = Users{connections=M.empty,
                   usersData=M.empty,
                   connToIds=M.empty,
                   userPids=M.empty,
                   userPidToIds=M.empty}
    scheduleTick $ S.tickMilliseconds aSettings
    loop state
    return ()


loop :: State -> Process State
loop state = handle >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [prepare handleTick,
                    prepare handleMoveTo,
                    prepare (request handleObjectsInfo),
                    prepare handleEnter,
                    prepare handleReconnection,
                    prepare handleEnterArea,
                    prepare handleIgnite,
                    prepare (request handleEcho),
                    prepare handleMonitorNotification,
                    matchUnknown (return state)]
        evalState = receiveWait handlers >>= evaluate
        handle = evalState `catches` logException state


uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connToIds . users) state M.! conn

userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

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

makeSelfPid :: Process AreaPid
makeSelfPid = liftM AreaPid getSelfPid

initConnection :: Connection -> State -> Process ()
initConnection conn state = do
    setArea conn =<< makeSelfPid
    now <- liftIO milliseconds
    sendCmd conn "init" $ object ["areaId" .= areaId state,
                                  "timestamp" .= now]

request :: (Serializable a, ToJSON b) =>
           (State -> (a, Connection) -> Process (Response b)) ->
           State -> ((a, Connection), RequestNumber) -> Process State
request h state ((a, conn), req) = do
    (resp, state') <- h state (a, conn)
    sendResponse conn req resp
    return state'







