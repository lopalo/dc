module Controller (inputHandler) where

import Control.Distributed.Process
import Connection (Connection, sendToClient)
import Data.Aeson (ToJSON, Value, Result(Success), fromJSON, decode, encode)
import qualified Data.ByteString.Lazy as B


type Command = (String, Value)

sendTo :: ToJSON a => Connection -> String -> a -> Process ()
sendTo conn cmd body = sendToClient conn $ encode (cmd, body)

inputHandler :: Connection -> B.ByteString -> Process ()
inputHandler conn input = do
    let Just (cmd, body) = decode input :: Maybe Command
    commandHandler cmd body conn


commandHandler :: String -> Value -> Connection -> Process ()
commandHandler "echo" body conn = do
    let Success txt = fromJSON body :: Result String
    sendTo conn "echo-reply" $ "Echo: " ++ txt



