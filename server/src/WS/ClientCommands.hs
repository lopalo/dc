{-# LANGUAGE OverloadedStrings #-}

module WS.ClientCommands (inputHandler) where

import Prelude hiding (log)

import Control.Distributed.Process
import Data.Aeson (Value(Null), Result(Success), fromJSON, decodeStrict)
import Data.String.Utils (startswith)

import WS.Connection (Connection, InputHandler, sendCmd, sendResponse, log)
import qualified Area.External as AE
import qualified User.External as UE
import User.Auth (Login, Password, SignUpData, logIn, signUp)
import Types (UserPid, AreaPid, RequestNumber, LogLevel(..), delPrefix)
import Utils (evaluate)
import Base.Logger (logException)
import qualified Settings as S


type Command = (String, Value, RequestNumber)


delPathPrefix :: String -> String -> String
delPathPrefix = delPrefix "."


commandHandler ::
    S.Settings -> String -> Value -> RequestNumber ->
    Connection -> Maybe UserPid -> Maybe AreaPid -> Process ()
--NOTE: strict evaluation of parsed data is required to catch a parse error!!!
--NOTE: 0 means no request
commandHandler _ "echo" body req conn _ _ = do
    let Success txt = fromJSON body :: Result String
    seq txt $ sendResponse conn req $ "Echo: " ++ txt
commandHandler settings "log-in" body 0 conn _ _ = do
    let Success (login, password) = fromJSON body :: Result (Login, Password)
    evaluate login
    evaluate password
    sendCmd conn "init" Null
    spawnLocal $ logIn login password conn $ S.user settings
    return ()
commandHandler settings "sign-up" body 0 conn _ _ = do
    let Success signUpData = fromJSON body :: Result SignUpData
    evaluate signUpData
    sendCmd conn "init" Null
    spawnLocal $ signUp signUpData conn $ S.user settings
    return ()
commandHandler _ path body req conn _ (Just areaPid)
    | "area." `startswith` path =
        AE.clientCmd areaPid ("area" `delPathPrefix` path) body req conn
commandHandler _ path body req conn (Just userPid) _
    | "user." `startswith` path =
        UE.clientCmd userPid ("user" `delPathPrefix` path) body req conn


inputHandler :: S.Settings -> InputHandler
inputHandler settings input conn userPid areaPid = do
    let Just (cmd, body, req) = decodeStrict input :: Maybe Command
        handler = commandHandler settings cmd body req conn userPid areaPid
    log Debug $ "Incoming command: " ++ cmd
    handler `catches` logException ()



