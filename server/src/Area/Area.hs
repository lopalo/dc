{-# LANGUAGE OverloadedStrings #-}

module Area.Area (areaProcess) where

import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.), (>>>))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Data.Aeson (object, (.=))
import Data.Lens.Strict ((^%=))
import Data.Lens.Partial.Common (getPL, setPL)
import Control.Distributed.Process
import Control.Distributed.Process.Extras (newTagPool)
import Control.Distributed.Process.Extras.Call (callResponse)

import WS.Connection (Connection, setArea)
import qualified DB.DB as DB
import Utils (milliseconds, safeReceive, evaluate)
import qualified Area.Settings as AS
import Types (AreaId, AreaPid(..), UserName)
import qualified User.External as UE
import qualified Area.Objects.User as U
import Area.Utils (sendCmd)
import Area.Misc (spawnUser, broadcastOwnerName)
import Area.Types
import Area.State
import Area.ClientCommands (handleClientCommand, handleClientReq)
import Area.Signal (
    Signal(Appearance, Disappearance),
    AReason(LogIn, Entry),
    DReason(LogOut)
    )
import Area.Tick (handleTick, scheduleTick)
import Area.Collision (emptyColliders)


handleEnter :: State -> (Enter, Connection) -> Process State
handleEnter state (Enter ua userPid login, conn)
    | uid `M.member` usersData (users state) =
        return state
    | otherwise = do
        userMonitorRef <- UE.monitorUser userPid
        let state' = addSig $ spawnUser uid $ insUsr state
            user = U.User{
                U.userId=uid,
                U.connection=conn,
                U.pid=userPid,
                U.monitorRef=userMonitorRef,
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
            insUsr = usersL ^%= insertUser user
            reason = if login then LogIn else Entry
            addSig = addSignal $ Appearance uid reason
        initConnection conn state'
        UE.syncState userPid $ U.userArea user
        return state'
    where uid = UE.userId ua


handleReconnection :: State -> (Reconnection, Connection) -> Process State
handleReconnection state (Reconnection uid, conn) =
    case getPL connectionPL state of
        Just oldConn -> do
            let state' = changeConnection state
                changeConnection =
                    (connectionPL `setPL` conn) >>>
                    (connectionIndexL' ^%= M.delete oldConn) >>>
                    (connectionIndexL' ^%= M.insert conn uid)
            evaluate state'
            initConnection conn state'
            return state'
        Nothing -> return state
    where
        connectionPL = userFieldPL uid U.connectionL
        connectionIndexL' = connectionIndexL . usersL


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state (ProcessMonitorNotification ref _ _) =
    return $
        case ref `M.lookup` userMonitorRefIndex (users state) of
            Just uid ->
                let delUser = usersL ^%= deleteUser uid
                    addSig = addSignal $ Disappearance (UId uid) LogOut
                in addSig $ delUser state
            Nothing -> state


handleGetOwner ::
    State -> GetOwner -> Process ((AreaId, Maybe UserName), State)
handleGetOwner state _ =
    return ((areaId state, ownerName state), state)


areaProcess :: AS.Settings -> AreaId -> Process ()
areaProcess aSettings aid = do
    objects <- DB.getAreaObjects aid =<< newTagPool
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
            signalBuffer=Seq.empty,
            signalsForBroadcast=Seq.empty
            }
        us = Users{
            usersData=M.empty,
            connectionIndex=M.empty,
            userMonitorRefIndex=M.empty
            }
        fromList :: Object o => [o] -> M.Map ObjId o
        fromList = M.fromList . map (\o -> (objId o, o))
    scheduleTick $ AS.tickMilliseconds aSettings
    broadcastOwnerName state
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        prepareCall h = callResponse (h state)
        handlers = [
            prepare handleTick,
            prepare handleClientCommand,
            prepare handleClientReq,
            prepare handleEnter,
            prepare handleReconnection,
            prepareCall handleGetOwner,
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


