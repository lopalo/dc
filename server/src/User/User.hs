{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module User.User (userProcess) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)

import Data.Aeson (ToJSON, Value, object, (.=))
import Control.Distributed.Process hiding (reconnect)
import Control.Distributed.Process.Extras (newTagPool)

import Utils (logInfo, logDebug, safeReceive, milliseconds, Ts)
import Types (UserPid(UserPid), UserId(UserId), UserName, AreaId)
import GlobalRegistry (globalRegister, globalWhereIs)
import qualified Connection as C
import qualified Settings as S
import qualified User.Settings as US
import qualified Area.External as AE
import qualified User.External as UE
import DB (putUser, getUser)
import User.Types


userArea :: User -> UE.UserArea
userArea usr =
    UE.UserArea{UE.userId=userId usr,
                UE.name=name usr,
                UE.speed=speed usr,
                UE.maxDurability=maxDurability usr,
                UE.durability=durability usr}


data State = State {user :: !User,
                    areas :: ![AreaId],
                    settings :: !US.Settings,
                    connection :: Maybe C.Connection,
                    disconnectTs :: Maybe Ts}


data Period = Period deriving (Generic, Typeable)
instance Binary Period

data Reconnection = Reconnection C.Connection deriving (Generic, Typeable)
instance Binary Reconnection


handleMonitorNotification :: State -> ProcessMonitorNotification
                             -> Process State
handleMonitorNotification state n@ProcessMonitorNotification{} = do
    let Just conn = connection state
    now <- liftIO milliseconds
    return $ if C.checkMonitorNotification conn n
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
    logDebug "User reconnected"
    return state{connection=Just conn, disconnectTs=Nothing}
    where usr = user state

handleSyncState :: State -> UE.SyncState -> Process State
handleSyncState state (UE.SyncState ua) = do
    let usr = user state
        usr' = usr{durability=UE.durability ua}
    putUser usr'
    logDebug $ printf "User '%s' synchronized" $ show $ userId usr'
    return state{user=usr'}


handleSwitchArea :: State -> UE.SwitchArea -> Process State
handleSwitchArea state (UE.SwitchArea newAreaId) = do
    let usr = user state
        conn = connection state
        oldAreaId = area usr
        usr' = usr{area=newAreaId}
    maybeOldAreaPid <- globalWhereIs $ show oldAreaId
    case maybeOldAreaPid of
        Just pid -> unlink pid
        Nothing -> return () -- a link exception will be received later
    tryToLinkToArea newAreaId conn
    return state{user=usr'}

userProcess :: UserName -> C.Connection -> S.Settings -> Process ()
userProcess userName conn globalSettings = do
    let uid = UserId userName
    maybeUserPid <- globalWhereIs (show uid)
    case maybeUserPid of
        Just pid -> do
            reconnect pid conn
            terminate
        Nothing -> return ()
    getSelfPid >>= globalRegister (show uid)
    res <- getUser uid =<< newTagPool
    let startArea = S.startArea globalSettings
        uSettings = S.user globalSettings
        usr = fromMaybe newUser res
        areaId = area usr
        maxDurability = US.initDurability uSettings
        newUser = User{userId=uid,
                       name=userName,
                       area=startArea,
                       speed=US.speed uSettings,
                       maxDurability=maxDurability,
                       durability=maxDurability}
        state = State{user=usr,
                      areas=S.areas globalSettings,
                      settings=uSettings,
                      connection=Just conn,
                      disconnectTs=Nothing}
    tryToLinkToArea areaId (Just conn)
    putUser usr
    initConnection conn state
    userPid <- makeSelfPid
    AE.enter areaId (userArea usr) userPid True conn
    logInfo $ "Login: " ++ userName
    runPeriodic $ US.periodMilliseconds uSettings
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [prepare handlePeriod,
                    prepare handleSyncState,
                    prepare handleSwitchArea,
                    prepare handleMonitorNotification,
                    prepare handleReconnection,
                    matchUnknown (return state)]

logout :: UserName -> Process ()
logout userName = do
    logInfo $ "Logout: " ++ userName
    selfPid <- getSelfPid
    exit selfPid ("logout" :: String)

sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("user." ++ cmd)

makeSelfPid :: Process UserPid
makeSelfPid = fmap UserPid getSelfPid

reconnect :: ProcessId -> C.Connection -> Process ()
reconnect pid conn = send pid (Reconnection conn)

tryToLinkToArea :: AreaId -> Maybe C.Connection -> Process ()
tryToLinkToArea areaId maybeConn = do
    maybeAreaPid <- globalWhereIs $ show areaId
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
    sendCmd conn "init" $ initClientInfo state

initClientInfo :: State -> Value
initClientInfo state =
    object ["userId" .= userId usr,
            "name" .= name usr,
            "areas" .= areas state]
    where usr = user state

runPeriodic :: Ts -> Process ProcessId
runPeriodic ms = do
    selfPid <- getSelfPid
    let time = ms * 1000
        period = forever $ do
            liftIO $ threadDelay time
            send selfPid Period
    spawnLocal $ link selfPid >> period


