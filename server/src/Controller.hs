module Controller (inputHandler) where


import Control.Distributed.Process
import Data.Aeson (Value, Result(Success), fromJSON, decode)
import Data.String.Utils (startswith)

import Connection (Connection, sendCmd, sendResponse, InputHandler)
import qualified Area.Area as A
import Types (UserId(UserId), UserPid, AreaPid, RequestNumber)
import Utils (delPrefix, logException, evaluate)
import qualified Settings as S


type Command = (String, Value, RequestNumber)

delPathPrefix :: String -> String -> String
delPathPrefix = delPrefix "."

commandHandler :: String -> Value -> RequestNumber ->
                  Connection -> Maybe UserPid -> Maybe AreaPid ->
                  Process ()
--NOTE: strict evaluation of parsed data is required to catch a parse error!!!
--NOTE: 0 means no request
commandHandler "echo" body req conn _ _ = do
    let Success txt = fromJSON body :: Result String
    seq txt $ sendResponse conn req $ "Echo: " ++ txt
commandHandler "login" body 0 conn _ _ = do
    let Success name = fromJSON body :: Result String
        userId = UserId name
        areaUserInfo = (userId, name)
    evaluate userId
    A.enter S.startArea areaUserInfo conn
commandHandler path body req conn _ (Just areaPid)
    | "area." `startswith` path =
        A.clientCmd areaPid ("area" `delPathPrefix` path) body req conn



--external interface

inputHandler :: InputHandler
inputHandler input conn userPid areaPid = do
    let Just (cmd, body, req) = decode input :: Maybe Command
        handler = commandHandler cmd body req conn userPid areaPid
    handler `catches` logException ()



