{-# LANGUAGE OverloadedStrings #-}

module Controller (inputHandler) where


import Control.Distributed.Process
import Data.Aeson (Value, Result(Success), fromJSON, decode, object, (.=))
import Data.String.Utils (startswith)

import Connection (Connection, sendCmd, sendResponse, InputHandler)
import qualified Area.Area as A
import Types (UserId(UserId), UserPid, AreaPid, RequestNumber)
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
commandHandler settings "echo" body req conn _ _ = do
    let Success txt = fromJSON body :: Result String
    seq txt $ sendResponse conn req $ "Echo: " ++ txt
commandHandler settings "login" body 0 conn _ _ = do
    let Success name = fromJSON body :: Result String
        userId = UserId name
        areaUserInfo = (userId, name)
    evaluate userId
    --TODO: send it from the user's component (user.init) and then do enter to an area
    sendCmd conn "init" $ object ["userId" .= userId,
                                  "name" .= name,
                                  "areas" .= S.areas settings]
    A.enter (S.startArea settings) areaUserInfo conn
commandHandler settings path body req conn _ (Just areaPid)
    | "area." `startswith` path =
        A.clientCmd areaPid ("area" `delPathPrefix` path) body req conn



--external interface

inputHandler :: S.Settings -> InputHandler
inputHandler settings input conn userPid areaPid = do
    let Just (cmd, body, req) = decode input :: Maybe Command
        handler = commandHandler settings cmd body req conn userPid areaPid
    handler `catches` logException ()



