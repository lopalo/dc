{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Base.Broadcaster (
    broadcasterProcess, isRunning,
    broadcast, prepareHandler
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M

import Control.Distributed.Process hiding (forward)
import Control.Distributed.Process.Serializable (Serializable)


data Broadcast = Broadcast [ProcessId] deriving (Generic, Typeable)
instance Binary Broadcast


broadcasterServiceName :: String
broadcasterServiceName = "broadcaster"


broadcasterProcess :: [Match ()] -> Process ()
broadcasterProcess handlers = do
    register broadcasterServiceName =<< getSelfPid
    forever $ receiveWait handlers'
    where handlers' = handlers ++ [matchUnknown (return ())]


--external interface


isRunning :: Process Bool
isRunning = isJust <$> whereis broadcasterServiceName


broadcast :: Serializable a => [ProcessId] -> a -> Process ()
broadcast pids payload = mapM_ s $ M.toList groupedPids
    where
        groupedPids = foldl group M.empty pids
        group groups pid =
            let nodeId = processNodeId pid
                pids' = M.findWithDefault [] nodeId groups
            in M.insert nodeId (pid : pids') groups
        s (nodeId, pids') =
            nsendRemote nodeId broadcasterServiceName payload'
            where payload' = (Broadcast pids', payload)


prepareHandler ::
    Serializable a => ((a -> Process()) -> a -> Process ()) -> Match ()
prepareHandler handler = match wrap
    where
        wrap (Broadcast localPids, payload) = handler forward payload
            where forward payload' = mapM_ (`unsafeSend` payload') localPids
