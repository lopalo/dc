{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module App.Connection (acceptConnection, InputHandler, Connection,
                       sendCmd, sendResponse, broadcastCmd, setUser,
                       setArea, close, monitorConnection,
                       checkMonitorNotification) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever, void)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B

import Data.Aeson(ToJSON, encode)
import Control.Distributed.Process
import qualified Control.Distributed.Process.Node as Node
import qualified Network.WebSockets as WS

import App.Types (RequestNumber, UserPid, AreaPid)
import App.Utils (evaluate, logDebug)


data Connection = Connection {output :: ProcessId, input :: ProcessId}
                  deriving (Eq, Ord, Generic, Typeable)
instance Binary Connection


type InputHandler = B.ByteString -> Connection -> Maybe UserPid ->
                    Maybe AreaPid -> Process ()


acceptConnection :: Node.LocalNode -> InputHandler -> WS.ServerApp
acceptConnection node inputHandler pending = do
    wsConn <- WS.acceptRequest pending
    outputPid <- Node.forkProcess node $ do
        let outputLoop = forever $ do
            ("send", outputData) <- expect :: Process (String, B.ByteString)
            logOutput outputData
            liftIO $ WS.sendTextData wsConn outputData
        outputLoop `finally` logDebug "Connection: output closed"
    Node.runProcess node $ do
        inputPid <- getSelfPid
        let conn = Connection outputPid inputPid
            final = do exit outputPid "closed"
                       logDebug "Connection: input closed"
            setUserPid (_, areaPid) userPid = return (Just userPid, areaPid)
            setAreaPid (userPid, _) areaPid = return (userPid, Just areaPid)
            updateState state = do
                ret <- receiveTimeout 0 [match (setUserPid state),
                                         match (setAreaPid state)]
                case ret of
                    Just state' -> updateState state'
                    Nothing -> return state
            inputLoop state = do
                inputData <- liftIO (WS.receiveData wsConn :: IO B.ByteString)
                logInput inputData
                state' <- updateState state
                uncurry (inputHandler inputData conn) state'
                inputLoop state'
        link outputPid >> inputLoop (Nothing, Nothing) `finally` final


logOutput :: B.ByteString -> Process ()
logOutput bytes = logDebug $ "Output: " ++ toString bytes

logInput :: B.ByteString -> Process ()
logInput bytes = logDebug $ "Input: " ++ toString bytes


--external interface
sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd conn cmd body = do
    evaluate body
    send (output conn) ("send", encode (cmd, body))

sendResponse :: ToJSON a => Connection -> RequestNumber -> a -> Process ()
sendResponse conn req = sendCmd conn $ "response:" ++ show req

broadcastCmd :: ToJSON a => [Connection] -> String -> a -> Process ()
broadcastCmd connections cmd body =
    mapM_ (\conn -> sendCmd conn cmd body) connections

setUser :: Connection -> UserPid -> Process ()
setUser (Connection _ inputPid) = send inputPid

setArea :: Connection -> AreaPid -> Process ()
setArea (Connection _ inputPid) = send inputPid

close :: Connection -> Process ()
close conn = exit (input conn) "close connection"

monitorConnection :: Connection -> Process ()
monitorConnection conn = void $ monitor $ output conn

checkMonitorNotification :: Connection -> ProcessMonitorNotification -> Bool
checkMonitorNotification conn (ProcessMonitorNotification _ pid _) =
    output conn == pid
