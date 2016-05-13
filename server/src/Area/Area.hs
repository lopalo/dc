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
import qualified DB.AreaDB as DB
import Utils (milliseconds, safeReceive, evaluate)
import qualified Area.Settings as AS
import Types (ServiceId, AreaId(..), AreaPid(..), UserName, AreaStatus(..))
import qualified User.External as UE
import qualified User.UserArea as UA
import qualified Area.Objects.User as U
import Area.Utils (sendCmd)
import Area.Misc (spawnUser, updateOwnerName)
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
                U.name=UA.name ua,
                U.pos=Pos 0 0,
                U.angle=0,
                U.speed=UA.speed ua,
                U.maxDurability=UA.maxDurability ua,
                U.durability=UA.durability ua,
                U.actions=[],
                U.size=UA.size ua,
                U.kills=UA.kills ua,
                U.deaths=UA.deaths ua,
                U.lastAttacker=Nothing,
                U.nextShootTs=0
                }
            insUsr = usersL ^%= insertUser user
            reason = if login then LogIn else Entry
            addSig = addSignal $ Appearance uid reason
        initConnection conn state'
        UE.syncState userPid $ U.userArea user
        return state'
    where uid = UA.userId ua


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


handleGetAreaStatus ::
    State -> GetAreaStatus -> Process (AreaStatus, State)
handleGetAreaStatus state _ =
    return (res, state)
    where
        amount getter = M.size $ getter state
        usrAmount = amount (usersData . users)
        amountLst = [
            usrAmount,
            amount gates,
            amount asteroids,
            amount controlPoints
            ]
        res = AreaStatus{
            statusAreaId=areaId state,
            userAmount=usrAmount,
            objectAmount=sum amountLst,
            tickDurationMs=maximum $ tickDurations state
            }


areaProcess :: AS.Settings -> ServiceId -> Int -> Process ()
areaProcess aSettings ident minReplicas = do
    objects <- DB.getAreaObjects aid minReplicas =<< newTagPool
    now <- liftIO milliseconds
    let state = State{
            areaId=aid,
            settings=aSettings,
            minDBReplicas=minReplicas,
            tickNumber=0,
            tickDurations=[],
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
    updateOwnerName state
    loop state
    where aid = AreaId ident


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
            prepareCall handleGetAreaStatus,
            prepare handleMonitorNotification,
            matchUnknown (return state)
            ]


makeSelfPid :: Process AreaPid
makeSelfPid = liftM AreaPid getSelfPid


initConnection :: Connection -> State -> Process ()
initConnection conn state = do
    setArea conn =<< makeSelfPid
    now <- liftIO milliseconds
    sendCmd conn "init" $ object [
        "areaId" .= areaId state,
        "timestamp" .= now,
        "global-positions" .= positions'
        ]
    where
        positions = AS.globalPositions $ settings state
        positions' = M.mapKeys (show . AreaId) positions


