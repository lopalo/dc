{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module User.User (userProcess) where

import Prelude hiding (log)
import Control.Monad (forever, when, unless)
import Control.Monad.Catch (onException)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import Data.Aeson (ToJSON, Value, object, (.=))
import Control.Distributed.Process hiding (reconnect, onException)
import Control.Distributed.Process.Extras (TagPool, newTagPool)
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)

import Utils (safeReceive, milliseconds)
import Types (
    Ts, UserPid(UserPid), UserId(UserId),
    UserName, AreaId, LogLevel(..)
    )
import Base.GlobalRegistry (globalRegister, globalWhereIs)
import qualified Base.GlobalCache as GC
import Base.Logger (log)
import qualified WS.Connection as C
import qualified User.Settings as US
import qualified Area.External as AE
import qualified User.External as UE
import DB.UserDB (putUser, getUser)
import User.Types
import User.State
import User.ClientCommands (handleClientCommand)


userArea :: User -> UE.UserArea
userArea usr = UE.UserArea{
    UE.userId=userId usr,
    UE.name=name usr,
    UE.speed=speed usr,
    UE.maxDurability=maxDurability usr,
    UE.durability=durability usr,
    UE.size=size usr,
    UE.kills=kills usr,
    UE.deaths=deaths usr
    }


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state n@ProcessMonitorNotification{} = do
    let Just conn = connection state
    now <- liftIO milliseconds
    return $
        if C.checkMonitorNotification conn n
            then state{connection=Nothing, disconnectTs=Just now}
            else state


handlePeriod :: State -> Period -> Process State
handlePeriod state Period = do
    let logoutMs = 1000 * US.logoutSeconds (settings state)
        userName = name $ user state
    now <- liftIO milliseconds
    case disconnectTs state of
        Just ts -> when (now - ts > logoutMs) $ logout userName
        Nothing -> return ()
    return state


handleReconnection :: State -> Reconnection -> Process State
handleReconnection state (Reconnection conn) = do
    case connection state of
        Just oldConn -> C.sendErrorAndClose oldConn "Reconnection"
        Nothing -> return ()
    initConnection conn state
    AE.reconnect (area usr) (userId usr) conn
    log Debug "User reconnected"
    return state{connection=Just conn, disconnectTs=Nothing}
    where usr = user state


handleSyncState :: State -> UE.SyncState -> Process State
handleSyncState state (UE.SyncState ua) = do
    let usr = user state
        usr' = usr{
            durability=UE.durability ua,
            kills=UE.kills ua,
            deaths=UE.deaths ua
            }
        minReplicas = US.minDBReplicas $ settings state
    putUser usr' minReplicas $ reqTagPool state
    log Debug $ printf "User '%s' synchronized" $ show $ userId usr'
    return state{user=usr'}


handleSwitchArea :: State -> SwitchArea -> Process State
handleSwitchArea state (SwitchArea newAreaId) = do
    let usr = user state
        conn = connection state
        tagPool = reqTagPool state
        oldAreaId = area usr
        usr' = usr{area=newAreaId}
    maybeOldAreaPid <- globalWhereIs (show oldAreaId) tagPool
    case maybeOldAreaPid of
        Just pid -> unlink pid
        Nothing -> return () -- a link exception will be received later
    tryToLinkToArea newAreaId conn tagPool
    return state{user=usr'}


handleUserMessage :: State -> UserMessage -> Process State
handleUserMessage state userMsg = do
    case connection state of
        Just conn ->
            sendCmd conn "add-messages" [userMsg]
        Nothing -> return ()
    return state{userMessages=userMessages state Seq.|> userMsg}


handleNewCacheValue :: State -> GC.NewCacheValue -> Process State
handleNewCacheValue state newCacheValue = do
    let GC.NewCacheValue (key, GC.AreaOwner maybeUserName) = newCacheValue
    case connection state of
        Just conn ->
            sendCmd conn "update-worldmap" $ M.singleton key maybeUserName
        Nothing -> return ()
    return state


userProcess :: UserName -> C.Connection -> US.Settings -> Process ()
userProcess userName conn userSettings = do
    let uid = UserId userName
        minReplicas = US.minDBReplicas userSettings
        onDBError = C.sendErrorAndClose conn "Cannot load user from DB"
    tagPool <- newTagPool
    maybeUserPid <- globalWhereIs (show uid) tagPool
    case maybeUserPid of
        Just pid -> do
            reconnect pid conn
            terminate
        Nothing -> return ()
    pid <- getSelfPid
    ok <- globalRegister (show uid) pid tagPool
    unless ok terminate
    res <- getUser uid minReplicas tagPool `onException` onDBError
    let startArea = US.startArea userSettings
        usr = fromMaybe newUser res
        areaId = area usr
        maxDur = US.initDurability userSettings
        mConn = Just conn
        newUser = User{
            userId=uid,
            name=userName,
            area=startArea,
            speed=US.speed userSettings,
            maxDurability=maxDur,
            durability=maxDur,
            size=US.size userSettings,
            kills=0,
            deaths=0
            }
        state = State{
            user=usr,
            settings=userSettings,
            connection=mConn,
            disconnectTs=Nothing,
            reqTagPool=tagPool,
            userMessages=Seq.empty
            }
    tryToLinkToArea areaId mConn tagPool
    putUser usr minReplicas tagPool
    initConnection conn state
    userPid <- makeSelfPid
    AE.enter areaId (userArea usr) userPid True conn
    log Info $ "Login: " ++ userName
    runPeriodic $ US.periodMilliseconds userSettings
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [
            prepare handlePeriod,
            prepare handleSyncState,
            prepare handleClientCommand,
            prepare handleUserMessage,
            prepare handleSwitchArea,
            prepare handleMonitorNotification,
            prepare handleReconnection,
            prepare handleNewCacheValue,
            matchUnknown (return state)
            ]


logout :: UserName -> Process ()
logout userName = do
    log Info $ "Logout: " ++ userName
    selfPid <- getSelfPid
    exit selfPid ("logout" :: String)


sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("user." ++ cmd)


makeSelfPid :: Process UserPid
makeSelfPid = fmap UserPid getSelfPid


reconnect :: ProcessId -> C.Connection -> Process ()
reconnect pid conn = send pid (Reconnection conn)


tryToLinkToArea :: AreaId -> Maybe C.Connection -> TagPool -> Process ()
tryToLinkToArea areaId maybeConn tagPool = do
    maybeAreaPid <- globalWhereIs (show areaId) tagPool
    case maybeAreaPid of
        Nothing -> do
            case maybeConn of
                Just conn -> C.sendErrorAndClose conn "Area is inaccessible"
                Nothing -> return ()
            terminate
        Just areaPid ->
            link areaPid


initConnection :: C.Connection -> State -> Process ()
initConnection conn state = do
    C.monitorConnection conn
    makeSelfPid >>= C.setUser conn
    res <- GC.getSubscribe GC.AreaOwnerTag $ reqTagPool state
    let areaOwners = M.fromList [(k, v) | (k, GC.AreaOwner v) <- res]
    sendCmd conn "init" $ initClientInfo state
    sendCmd conn "add-messages" $ toList $ userMessages state
    sendCmd conn "update-worldmap" areaOwners


initClientInfo :: State -> Value
initClientInfo state =
    object [
        "userId" .= userId usr,
        "name" .= name usr
        ]
    where usr = user state


runPeriodic :: Ts -> Process ProcessId
runPeriodic ms = do
    selfPid <- getSelfPid
    let period = forever $ sleepFor ms Millis >> send selfPid Period
    spawnLocal $ link selfPid >> period


