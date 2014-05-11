module Controller (inputHandler) where

import qualified Data.ByteString.Lazy as B

import Control.Distributed.Process
import Data.Aeson (ToJSON, Value, Result(Success), fromJSON, decode, encode)

import Connection (Connection, sendToClient, InputHandler)
import Types (UserPid, AreaPid)


type Command = (String, Value)

sendTo :: ToJSON a => Connection -> String -> a -> Process ()
sendTo conn cmd body = sendToClient conn $ encode (cmd, body)

inputHandler :: InputHandler
inputHandler input conn userPid areaPid = do
    let Just (cmd, body) = decode input :: Maybe Command
    commandHandler cmd body conn userPid areaPid


commandHandler :: String -> Value -> Connection -> Maybe UserPid
                  -> Maybe AreaPid -> Process ()
commandHandler "echo" body conn _ _ = do
    let Success txt = fromJSON body :: Result String
    sendTo conn "echo-reply" $ "Echo: " ++ txt
commandHandler "login" body conn _ areaPid = do
    let Success name = fromJSON body :: Result String
    --TODO: enter to area
    return ()



