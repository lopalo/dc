{-# LANGUAGE OverloadedStrings #-}

module Area.Area (areaProcess) where

import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.), (>>>))
import qualified Data.Map.Strict as M

import Data.Aeson(ToJSON, object, (.=))
import Data.Lens.Common ((^$), (^%=))
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea, sendResponse)
import Utils (milliseconds, safeReceive, evaluate)
import qualified Settings as S
import Types (UserPid(..), AreaId, AreaPid(..), RequestNumber)
import qualified User.External as UE
import qualified Area.User as U
import Area.Utils (sendCmd)
import Area.Types
import Area.State
import Area.ClientCommands
import Area.Event (Event(Appearance, Disappearance),
                   AReason(LogIn, Entry),
                   DReason(LogOut))
import Area.Tick (handleTick, scheduleTick)



handleEnter :: State -> (Enter, Connection) -> Process State
handleEnter state (Enter ua userPid login, conn) = do
    let uid = UE.userId ua
        enterPos = (S.enterPos . settings) state
        user = U.User{U.userId=uid,
                      U.name=UE.name ua,
                      U.pos=uncurry Pos enterPos,
                      U.angle=0,
                      U.speed=UE.speed ua,
                      U.durability=UE.durability ua,
                      U.actions=[]}
        addUsr = users' ^%= addUser uid conn userPid user
        reason = if login then LogIn else Entry
        addEvent = events' ^%= (Appearance uid reason :)
        state' = addEvent $ addUsr state
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
    return $ case UserPid pid `M.lookup` userPidToIds (users state) of
        Just uid ->
            let delUser = users' ^%= deleteUser uid
                addEvent = events' ^%= (Disappearance uid LogOut :)
            in addEvent $ delUser state
        Nothing -> state



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
    register aid =<< getSelfPid
    scheduleTick $ S.tickMilliseconds aSettings
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
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







