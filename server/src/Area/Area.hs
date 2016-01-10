{-# LANGUAGE OverloadedStrings #-}

module Area.Area (areaProcess) where

import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.), (>>>))
import qualified Data.Map.Strict as M

import Data.Aeson (object, (.=))
import Data.Lens.Strict ((^$), (^%=))
import Control.Distributed.Process
import Control.Distributed.Process.Extras (newTagPool)

import WS.Connection (Connection, setArea)
import qualified DB.DB as DB
import Utils (milliseconds, safeReceive, evaluate, logError)
import qualified Area.Settings as AS
import Types (UserPid(..), AreaId, AreaPid(..))
import qualified User.External as UE
import qualified Area.Objects.User as U
import Area.Utils (sendCmd)
import Area.Misc (spawnUser)
import Area.Types
import Area.State
import Area.ClientCommands
import Area.Signal (
    Signal(Appearance, Disappearance),
    AReason(LogIn, Entry),
    DReason(LogOut)
    )
import Area.Tick (handleTick, scheduleTick)
import Area.Collision (emptyColliders)


handleEnter :: State -> (Enter, Connection) -> Process State
handleEnter state (Enter ua userPid login, conn) = do
    let uid = UE.userId ua
        user = U.User{
            U.userId=uid,
            U.name=UE.name ua,
            U.pos=Pos 0 0,
            U.angle=0,
            U.speed=UE.speed ua,
            U.maxDurability=UE.maxDurability ua,
            U.durability=UE.durability ua,
            U.actions=[],
            U.size=UE.size ua,
            U.kills=UE.kills ua,
            U.deaths=UE.deaths ua,
            U.lastAttacker=Nothing
            }
        insUsr = usersL ^%= insertUser uid conn userPid user
        reason = if login then LogIn else Entry
        addSig = addSignal $ Appearance uid reason
        state' = addSig $ spawnUser uid $ insUsr state
    if uid `M.member` userPids (users state)
        then return state
        else do
            UE.monitorUser userPid
            initConnection conn state'
            UE.syncState userPid $ U.userArea user
            return state'


handleReconnection :: State -> (Reconnection, Connection) -> Process State
handleReconnection state (Reconnection uid, conn) = do
    let connectionsL' = connectionsL . usersL
        connToIdsL' = connToIdsL . usersL
        oldConn = (connectionsL' ^$ state) M.! uid
        changeConnection =
            (connectionsL' ^%= M.insert uid conn) >>>
            (connToIdsL' ^%= M.delete oldConn) >>>
            (connToIdsL' ^%= M.insert conn uid)
        state' = changeConnection state
    evaluate state'
    initConnection conn state'
    return state'


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state (ProcessMonitorNotification ref pid _) = do
    unmonitor ref
    return $
        case UserPid pid `M.lookup` userPidToIds (users state) of
            Just uid ->
                let delUser = usersL ^%= deleteUser uid
                    addSig = addSignal $ Disappearance (UId uid) LogOut
                in addSig $ delUser state
            Nothing -> state


areaProcess :: AS.Settings -> AreaId -> Process ()
areaProcess aSettings aid = do
    res <- DB.getAreaObjects aid =<< newTagPool
    objects <-
        case res of
            Just objects -> return objects
            Nothing -> do
                logError $ "Cannot load " ++ show aid
                terminate
                return DB.emptyAreaObjects
    now <- liftIO milliseconds
    let state = State{
            areaId=aid,
            settings=aSettings,
            tickNumber=0,
            currentTs=now,
            users=us,
            gates=fromList $ DB.gates objects,
            asteroids=fromList $ DB.asteroids objects,
            controlPoints=fromList $ DB.controlPoints objects,
            colliders=emptyColliders,
            signalBuffer=[],
            signalsForBroadcast=[]
            }
        us = Users{
            connections=M.empty,
            usersData=M.empty,
            connToIds=M.empty,
            userPids=M.empty,
            userPidToIds=M.empty
            }
        fromList :: Object o => [o] -> M.Map ObjId o
        fromList = M.fromList . map (\o -> (objId o, o))
    scheduleTick $ AS.tickMilliseconds aSettings
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [
            prepare handleTick,
            prepare handleClientCommand,
            prepare handleClientReq,
            prepare handleEnter,
            prepare handleReconnection,
            prepare handleMonitorNotification,
            matchUnknown (return state)
            ]


makeSelfPid :: Process AreaPid
makeSelfPid = liftM AreaPid getSelfPid


initConnection :: Connection -> State -> Process ()
initConnection conn state = do
    setArea conn =<< makeSelfPid
    now <- liftIO milliseconds
    sendCmd conn "init" $ object
        ["areaId" .= areaId state, "timestamp" .= now]


