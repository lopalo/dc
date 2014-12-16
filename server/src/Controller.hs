{-# LANGUAGE OverloadedStrings #-}

module Controller (inputHandler) where


import Control.Distributed.Process
import Data.Aeson (Value(Null), Result(Success), fromJSON, decode)
import Data.String.Utils (startswith)

import Connection (Connection, sendCmd, sendResponse, InputHandler)
import qualified Area.Area as A
import User.User (userProcess)
import Types (UserPid, AreaPid, RequestNumber)
import Utils (delPrefix, logException, evaluate)
import qualified Settings as S


type Command = (String, Value, RequestNumber)

delPathPrefix :: String -> String -> String
delPathPrefix = delPrefix "."

commandHandler :: S.Settings -> String -> Value -> RequestNumber ->
                  Connection -> Maybe UserPid -> Maybe AreaPid ->
                  Process ()
--NOTE: strict evaluation of parsed data is required to catch a parse error!!!
--NOTE: 0 means no request
commandHandler _ "echo" body req conn _ _ = do
    let Success txt = fromJSON body :: Result String
    seq txt $ sendResponse conn req $ "Echo: " ++ txt
commandHandler settings "login" body 0 conn _ _ = do
    let Success name = fromJSON body :: Result String
    evaluate name
    sendCmd conn "init" Null
    spawnLocal $ userProcess name conn settings
    return ()
commandHandler _ path body req conn _ (Just areaPid)
    | "area." `startswith` path =
        A.clientCmd areaPid ("area" `delPathPrefix` path) body req conn
    --FIXME: Non-exhaustive patterns on login



--external interface

inputHandler :: S.Settings -> InputHandler
inputHandler settings input conn userPid areaPid = do
    let Just (cmd, body, req) = decode input :: Maybe Command
        handler = commandHandler settings cmd body req conn userPid areaPid
    handler `catches` logException ()



