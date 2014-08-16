{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Connection (acceptConnection, InputHandler, Connection,
                   sendCmd, sendResponse, broadcastCmd,
                   setUser, setArea, close) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B

import Data.Aeson(ToJSON, encode)
import Control.Distributed.Process
import qualified Control.Distributed.Process.Node as Node
import qualified Network.WebSockets as WS

import Types (RequestNumber, UserPid, AreaPid)
import Utils (logDebug)


data Connection = Connection {output :: ProcessId, input :: ProcessId}
                  deriving (Eq, Ord, Generic, Typeable)
instance Binary Connection


type ConnState = (Maybe UserPid, Maybe AreaPid)

type InputHandler = B.ByteString -> Connection -> Maybe UserPid ->
                    Maybe AreaPid -> Process ()


acceptConnection :: Node.LocalNode -> InputHandler -> WS.ServerApp
acceptConnection node inputHandler pending = do
    wsConn <- WS.acceptRequest pending
    outputPid <- Node.forkProcess node $ forever $ do
        ("send", outputData) <- expect :: Process (String, B.ByteString)
        logOutput outputData
        liftIO $ WS.sendTextData wsConn outputData
    Node.runProcess node $ do
        inputPid <- getSelfPid
        let conn = Connection outputPid inputPid
            setUserPid :: ConnState -> UserPid -> Process ConnState
            setUserPid state _ = return state --TODO: implement
            setAreaPid :: ConnState -> AreaPid -> Process ConnState
            setAreaPid (userPid, _) areaPid = return (userPid, Just areaPid)
            loop :: ConnState -> Process ()
            loop state = do
                inputData <- liftIO (WS.receiveData wsConn :: IO B.ByteString)
                logInput inputData
                ret <- receiveTimeout 0 [match (setUserPid state),
                                         match (setAreaPid state)]
                let state' = fromMaybe state ret
                uncurry (inputHandler inputData conn) state'
                loop state'
            final = exit outputPid "closed" >> logDebug "Connection closed"
        link outputPid >> loop (Nothing, Nothing) `finally` final


logOutput :: B.ByteString -> Process ()
logOutput bytes = logDebug $ "Output: " ++ toString bytes

logInput :: B.ByteString -> Process ()
logInput bytes = logDebug $ "Input: " ++ toString bytes


--external interface
sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd conn cmd body =
    send (output conn) ("send", encode (cmd, body))

sendResponse :: ToJSON a => Connection -> RequestNumber -> a -> Process ()
sendResponse conn req = sendCmd conn $ "response:" ++ (show req)

broadcastCmd :: ToJSON a => [Connection] -> String -> a -> Process ()
broadcastCmd connections cmd body =
    mapM_ (\conn -> sendCmd conn cmd body) connections

setUser :: Connection -> UserPid -> Process ()
setUser (Connection _ inputPid) = send inputPid

setArea :: Connection -> AreaPid -> Process ()
setArea (Connection _ inputPid) = send inputPid

close :: Connection -> Process ()
close conn = exit (input conn) "close connection"



