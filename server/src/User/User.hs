{-# LANGUAGE OverloadedStrings #-}

module User.User (userProcess) where

import Control.Monad (void, when)

import Data.Aeson(ToJSON, object, (.=))
import Control.Distributed.Process

import Utils (logException, logInfo, evaluate)
import Types (Ts, UserPid(UserPid), UserId(UserId), UserName, AreaId)
import qualified Connection as C
import qualified Settings as S
import qualified Area.Area as A
import qualified User.External as UE


data User = User {userId :: UserId,
                  name :: UserName,
                  area :: AreaId,
                  speed :: Int, --units per second
                  durability :: Int}


userArea :: User -> AreaId -> UE.UserArea
userArea usr aid =
    UE.UserArea{UE.userId=userId usr,
                UE.name=name usr,
                UE.area=aid,
                UE.speed=speed usr,
                UE.durability=durability usr}



data State = State {user :: User,
                    connection :: Maybe C.Connection,
                    disconnectTs :: Maybe Ts}


handleMonitorNotification :: State -> ProcessMonitorNotification
                             -> Process State
handleMonitorNotification state n@ProcessMonitorNotification{} = do
    let userName = name $ user state
    case connection state of
        Just conn -> when (C.checkMonitorNotification conn n) (logout userName)
        Nothing -> return ()
    return state

userProcess :: UserName -> C.Connection -> S.Settings -> Process ()
userProcess userName conn settings = do
    let uSettings = S.user settings
        uid = UserId userName
        currentArea = S.startArea settings
    selfPid <- getSelfPid
    --TODO: if the user exists, just update his connection and send "init"
    register (show uid) selfPid
    C.monitorConnection conn
    sendCmd conn "init" $ object ["userId" .= uid,
                                  "name" .= userName,
                                  "areas" .= S.areas settings]
    let userPid = UserPid selfPid
        usr = User{userId=uid,
                   name=userName,
                   area=currentArea,
                   speed=S.speed uSettings,
                   durability=S.initDurability uSettings}
        state = State{user=usr,
                      connection=Just conn,
                      disconnectTs=Nothing}
    C.setUser conn userPid
    A.enter currentArea (userArea usr) userPid conn
    logInfo $ "Login: " ++ userName
    void $ loop state


loop :: State -> Process State
loop state = handle >>= loop
    --TODO: implement a session
    where
        prepare h = match (h state)
        --NOTE: handlers are matched by a type
        handlers = [prepare handleMonitorNotification,
                    matchUnknown (return state)]
        evalState = receiveWait handlers >>= evaluate
        handle = evalState `catches` logException state

logout :: UserName -> Process ()
logout userName = do
    logInfo $ "Logout: " ++ userName
    selfPid <- getSelfPid
    exit selfPid ("logout" :: String)

sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("user." ++ cmd)
