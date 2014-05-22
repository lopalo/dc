{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Connection (acceptConnection, InputHandler, Connection,
                   sendCmd, broadcastCmd, setUser, setArea,
                   close) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B

import Data.Aeson(ToJSON, encode)
import Control.Distributed.Process
import qualified Control.Distributed.Process.Node as Node
import qualified Network.WebSockets as WS

import Types (UserPid, AreaPid)


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
                let state' = case ret of
                        Nothing -> state
                        Just newState -> newState
                inputHandler inputData conn (fst state') (snd state')
                loop state'
            final = exit outputPid "closed" >> say "Connection closed"
        link outputPid >> loop (Nothing, Nothing) `finally` final


logOutput :: B.ByteString -> Process ()
logOutput bytes = say $ "Output: " ++ toString bytes

logInput :: B.ByteString -> Process ()
logInput bytes = say $ "Input: " ++ toString bytes


--external interface
sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd (Connection outputPid _) cmd body =
    send outputPid ("send", (encode (cmd, body)))

broadcastCmd :: ToJSON a => [Connection] -> String -> a -> Process ()
broadcastCmd connections cmd body =
    mapM_ (\conn -> sendCmd conn cmd body) connections

setUser :: Connection -> UserPid -> Process ()
setUser (Connection _ inputPid) userPid = send inputPid userPid

setArea :: Connection -> AreaPid -> Process ()
setArea (Connection _ inputPid) areaPid = send inputPid areaPid

close :: Connection -> Process ()
close (Connection _ inputPid) = exit inputPid "close connection"



