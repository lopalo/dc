{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Broadcaster (Broadcast, broadcast, localBroadcast) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import qualified Data.Map.Strict as M

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)


data Broadcast = Broadcast [ProcessId] deriving (Generic, Typeable)
instance Binary Broadcast


brokenBroadcast :: Serializable a => [ProcessId] -> a -> Process ()
brokenBroadcast pids payload = mapM_ s $ M.elems groupedPids
    --TODO: fix
    where
        groupedPids = foldl group M.empty pids
        group groups pid =
            let nodeId = processNodeId pid
                pids' = M.findWithDefault [] nodeId groups
            in M.insert nodeId (pid : pids') groups
        s (pid:pids') =
            send pid (Broadcast pids', payload)
        s [] = return ()


localBroadcast :: Serializable a => Broadcast -> a -> Process ()
localBroadcast (Broadcast localPids) payload =
    mapM_ (`unsafeSend` payload) localPids


broadcast :: Serializable a => [ProcessId] -> a -> Process ()
broadcast pids payload =
    --TODO: delete
    mapM_ (`send` payload) pids


