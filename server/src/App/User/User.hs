{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module App.User.User (userProcess) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)

import Data.Aeson(ToJSON, Value, object, (.=))
import Control.Distributed.Process hiding (reconnect)
import Control.Distributed.Process.Extras (newTagPool)

import App.Utils (logInfo, logDebug, safeReceive, milliseconds, Ts)
import App.Types (UserPid(UserPid), UserId(UserId), UserName, AreaId)
import qualified App.Connection as C
import qualified App.Settings as S
import qualified App.Area.External as AE
import qualified App.User.External as UE
import App.DB (putUser, getUser)
import App.User.Types


userArea :: User -> AreaId -> UE.UserArea
userArea usr aid =
    UE.UserArea{UE.userId=userId usr,
                UE.name=name usr,
                UE.area=aid,
                UE.speed=speed usr,
                UE.durability=durability usr}


data State = State {user :: !User,
                    areas :: ![AreaId],
                    settings :: !S.UserSettings,
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
    let logoutMs = 1000 * S.logoutSeconds (settings state)
        userName = name $ user state
    now <- liftIO milliseconds
    case disconnectTs state of
        Just ts -> when (now - ts > logoutMs) $ logout userName
        Nothing -> return ()
    return state

handleReconnection :: State -> Reconnection -> Process State
handleReconnection state (Reconnection conn) = do
    case connection state of
        Just oldConn -> C.close oldConn
        Nothing -> return ()
    initConnection conn state
    AE.reconnect (area usr) (userId usr) conn
    logDebug "User reconnected"
    return state{connection=Just conn, disconnectTs=Nothing}
    where usr = user state

handleSyncState :: State -> UE.SyncState -> Process State
handleSyncState state (UE.SyncState ua) = do
    let usr = user state
        usr' = usr{area=UE.area ua, durability=UE.durability ua}
    putUser usr'
    logDebug $ printf "User '%s' synchronized" $ show $ userId usr'
    return state{user=usr'}

userProcess :: UserName -> C.Connection -> S.Settings -> Process ()
userProcess userName conn globalSettings = do
    let uid = UserId userName
    userProc <- whereis (show uid)
    case userProc of
        Just pid -> do
            reconnect pid conn
            terminate
        Nothing -> return ()
    getSelfPid >>= register (show uid)
    res <- getUser uid =<< newTagPool
    let currentArea = S.startArea globalSettings
        uSettings = S.user globalSettings
        usr = fromMaybe newUser res
        newUser = User{userId=uid,
                       name=userName,
                       area=currentArea,
                       speed=S.speed uSettings,
                       durability=S.initDurability uSettings}
        state = State{user=usr,
                      areas=S.areas globalSettings,
                      settings=uSettings,
                      connection=Just conn,
                      disconnectTs=Nothing}
    putUser usr
    initConnection conn state
    userPid <- makeSelfPid
    AE.enter (area usr) (userArea usr) userPid True conn
    logInfo $ "Login: " ++ userName
    runPeriodic $ S.periodMilliseconds uSettings
    loop state


loop :: State -> Process ()
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [prepare handlePeriod,
                    prepare handleSyncState,
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


