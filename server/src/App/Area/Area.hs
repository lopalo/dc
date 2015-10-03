{-# LANGUAGE OverloadedStrings #-}

module App.Area.Area (areaProcess) where

import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.), (>>>))
import qualified Data.Map.Strict as M

import Data.Aeson (object, (.=))
import Data.Lens.Strict ((^$), (^%=))
import Control.Distributed.Process

import App.Connection (Connection, setArea)
import App.GlobalRegistry (globalRegister)
import App.Utils (milliseconds, safeReceive, evaluate)
import qualified App.Settings as S
import App.Types (UserPid(..), AreaId, AreaPid(..))
import qualified App.User.External as UE
import qualified App.Area.User as U
import App.Area.Utils (sendCmd)
import App.Area.Types
import App.Area.State
import App.Area.ClientCommands
import App.Area.Signal (Signal(Appearance, Disappearance),
                        AReason(LogIn, Entry),
                        DReason(LogOut))
import App.Area.Tick (handleTick, scheduleTick)



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
        addUsr = usersL ^%= addUser uid conn userPid user
        reason = if login then LogIn else Entry
        addSig = addSignal $ Appearance uid reason
        state' = addSig $ addUsr state
    UE.monitorUser userPid
    initConnection conn state'
    UE.syncState userPid $ U.userArea user
    return state'

handleReconnection :: State -> (Reconnection, Connection) -> Process State
handleReconnection state (Reconnection uid, conn) = do
    let connectionsL' = connectionsL . usersL
        connToIdsL' = connToIdsL . usersL
        oldConn = (connectionsL' ^$ state) M.! uid
        changeConnection = (connectionsL' ^%= M.insert uid conn) >>>
                           (connToIdsL' ^%= M.delete oldConn) >>>
                           (connToIdsL' ^%= M.insert conn uid)
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
            let delUser = usersL ^%= deleteUser uid
                addSig = addSignal $ Disappearance (UId uid) LogOut
            in addSig $ delUser state
        Nothing -> state



areaProcess :: S.AreaSettings -> AreaId -> Process ()
areaProcess aSettings aid = do
    let state = State{areaId=aid,
                      settings=aSettings,
                      tickNumber=0,
                      users=us,
                      signalBuffer=[],
                      signalsForBroadcast=[]}
        us = Users{connections=M.empty,
                   usersData=M.empty,
                   connToIds=M.empty,
                   userPids=M.empty,
                   userPidToIds=M.empty}
    globalRegister aid =<< getSelfPid
    scheduleTick $ S.tickMilliseconds aSettings
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [prepare handleTick,
                    prepare handleClientCommand,
                    prepare handleClientReq,
                    prepare handleEnter,
                    prepare handleReconnection,
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


