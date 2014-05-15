module Controller (inputHandler) where

import Control.Distributed.Process
import Data.Aeson (Value, Result(Success), fromJSON, decode)

import Connection (Connection, sendCmd, InputHandler)
import Area (enter, startArea)
import Types (UserId(UserId), UserPid, AreaPid)


type Command = (String, Value)

commandHandler :: String -> Value -> Connection -> Maybe UserPid
                  -> Maybe AreaPid -> Process ()
commandHandler "echo" body conn _ _ = do
    let Success txt = fromJSON body :: Result String
    sendCmd conn "echo-reply" $ "Echo: " ++ txt
commandHandler "login" body conn _ _ = do
    let Success name = fromJSON body :: Result String
        userId = UserId name
    enter startArea userId conn name


--external interface

inputHandler :: InputHandler
inputHandler input conn userPid areaPid = do
    let Just (cmd, body) = decode input :: Maybe Command
    commandHandler cmd body conn userPid areaPid



