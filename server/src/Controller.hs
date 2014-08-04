module Controller (inputHandler) where


import Control.Distributed.Process
import Data.Aeson (Value, Result(Success), fromJSON, decode)
import Data.String.Utils (startswith)

import Connection (Connection, sendCmd, InputHandler)
import qualified Area.Area as A
import Types (UserId(UserId), UserPid, AreaPid)
import Utils (delPrefix, logException, evaluate)
import qualified Settings as S


type Command = (String, Value)

delPathPrefix :: String -> String -> String
delPathPrefix = delPrefix "."

--TODO: implement request-response
commandHandler :: String -> Value -> Connection -> Maybe UserPid
                  -> Maybe AreaPid -> Process ()
--NOTE: strict evaluation of parsed data is required to catch a parse error!!!
commandHandler "echo" body conn _ _ = do
    let Success txt = fromJSON body :: Result String
    seq txt $ sendCmd conn "echo-reply" $ "Echo: " ++ txt
commandHandler "login" body conn _ _ = do
    let Success name = fromJSON body :: Result String
        userId = UserId name
        areaUserInfo = (userId, name)
    evaluate userId
    A.enter S.startArea areaUserInfo conn
commandHandler path body conn _ (Just areaPid)
    | "area." `startswith` path = do
        A.clientCmd areaPid ("area" `delPathPrefix` path) body conn



--external interface

inputHandler :: InputHandler
inputHandler input conn userPid areaPid = do
    let Just (cmd, body) = decode input :: Maybe Command
        handler = commandHandler cmd body conn userPid areaPid
    handler `catches` logException ()



