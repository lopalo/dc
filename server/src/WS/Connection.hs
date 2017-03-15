{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module WS.Connection (
    acceptConnection, InputHandler, Connection,
    sendCmd, sendResponse, broadcastCmd,
    broadcastBeginMultipart, broadcastEndMultipart,
    setUser, setArea, close, monitorConnection,
    checkMonitorNotification, sendErrorAndClose,
    connectionBroadcastHandlers, log
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (forever, void)
import Control.Monad.Catch (finally)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Data.Aeson (ToJSON, encode)
import Control.Distributed.Process hiding (finally)
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)
import qualified Control.Distributed.Process.Node as Node
import qualified Network.WebSockets as WS

import Base.Broadcaster (broadcast, prepareHandler)
import Types (RequestNumber, UserPid(..), AreaPid, LogLevel(..))
import Utils (evaluate)
import qualified Base.Logger as L


data Connection =
    Connection {output :: ProcessId, input :: ProcessId}
    deriving (Eq, Ord, Generic, Typeable)
instance Binary Connection


type InputHandler =
    B.ByteString -> Connection -> Maybe UserPid -> Maybe AreaPid -> Process ()


acceptConnection :: Node.LocalNode -> InputHandler -> WS.ServerApp
acceptConnection node inputHandler pending = do
    wsConn <- WS.acceptRequest pending
    outputPid <-
        Node.forkProcess node $ do
            let handleOutput bs =
                    liftIO $ WS.sendTextData wsConn (bs :: LB.ByteString)
                outputLoop =
                    forever $ receiveWait [
                        match handleOutput
                        ]
            outputLoop `finally` log Debug "Output closed"
    Node.runProcess node $ do
        inputPid <- getSelfPid
        let conn = Connection outputPid inputPid
            final = do
                exit outputPid "closed"
                log Debug "Input closed"
            setUserPid (_, areaPid) userPid@(UserPid pid) = do
                link pid
                return (Just userPid, areaPid)
            setAreaPid (userPid, _) areaPid = return (userPid, Just areaPid)
            updateState state = do
                ret <-
                    receiveTimeout 0 [
                        match (setUserPid state),
                        match (setAreaPid state)
                        ]
                case ret of
                    Just state' -> updateState state'
                    Nothing -> return state
            inputLoop state = do
                inputData <- liftIO (WS.receiveData wsConn :: IO B.ByteString)
                state' <- updateState state
                uncurry (inputHandler inputData conn) state'
                inputLoop state'
        link outputPid >> inputLoop (Nothing, Nothing) `finally` final


log :: LogLevel -> String -> Process ()
log level txt = L.log level $ "WS - " ++ txt


handleBroadcastOutput ::
    (LB.ByteString -> Process()) -> LB.ByteString -> Process ()
handleBroadcastOutput = ($)


--external interface
sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd conn cmd body = do
    --log Debug $ "Outgoing command: " ++ cmd
    evaluate body
    send (output conn) (encode (cmd, body))


sendResponse :: ToJSON a => Connection -> RequestNumber -> a -> Process ()
sendResponse conn req = sendCmd conn $ "response:" ++ show req


broadcastCmd :: ToJSON a => [Connection] -> String -> a -> Process ()
broadcastCmd connections cmd body = do
    --log Debug $ "Outgoing command: " ++ cmd
    evaluate body
    broadcast (map output connections) payload
    where payload = encode (cmd, body)


broadcastBeginMultipart :: [Connection] -> String -> Process ()
broadcastBeginMultipart connections =
    broadcastCmd connections "begin-multipart"


broadcastEndMultipart :: [Connection] -> String -> Process ()
broadcastEndMultipart connections = broadcastCmd connections "end-multipart"


setUser :: Connection -> UserPid -> Process ()
setUser (Connection _ inputPid) = send inputPid


setArea :: Connection -> AreaPid -> Process ()
setArea (Connection _ inputPid) = send inputPid


close :: Connection -> Process ()
close conn = exit (input conn) "close connection"


sendErrorAndClose :: Connection -> String -> Process ()
sendErrorAndClose conn errorMsg = do
    sendCmd conn "error" errorMsg
    spawnLocal $ sleepFor 5 Seconds >> close conn
    return ()


monitorConnection :: Connection -> Process ()
monitorConnection conn = void $ monitor $ output conn


checkMonitorNotification :: Connection -> ProcessMonitorNotification -> Bool
checkMonitorNotification conn (ProcessMonitorNotification _ pid _) =
    output conn == pid


connectionBroadcastHandlers :: [Match ()]
connectionBroadcastHandlers = [prepareHandler handleBroadcastOutput]

