module Connection (acceptConnection, Connection, sendToClient) where

import Control.Monad (forever)
import Control.Distributed.Process
import Data.ByteString.Lazy.UTF8 (toString)

import qualified Data.ByteString.Lazy as B
import qualified Control.Distributed.Process.Node as Node
import qualified Network.WebSockets as WS

newtype Connection = Connection ProcessId


acceptConnection :: Node.LocalNode ->
                    (Connection -> B.ByteString -> Process ()) ->
                    WS.ServerApp
acceptConnection node inputHandler pending = do
    wsConn <- WS.acceptRequest pending
    outputPid <- Node.forkProcess node $ forever $ do
         --TODO: accept message to close
        ("send", output) <- expect :: Process (String, B.ByteString)
        logOutput output
        liftIO $ WS.sendTextData wsConn output
    let conn = Connection outputPid
    Node.runProcess node $ do
        let loop = forever $ do
            input <- liftIO (WS.receiveData wsConn :: IO B.ByteString)
            logInput input
            inputHandler conn input
        loop `finally` exit outputPid "Connection closed"


sendToClient :: Connection -> B.ByteString -> Process ()
sendToClient (Connection outputPid) bytes = send outputPid ("send", bytes)

logOutput :: B.ByteString -> Process ()
logOutput bytes = say $ "Output: " ++ toString bytes

logInput :: B.ByteString -> Process ()
logInput bytes = say $ "Input: " ++ toString bytes


