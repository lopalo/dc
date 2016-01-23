{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module NodeControl.NodeControl (
    GetNodeInfo(..), NodeInfo(..),
    nodeControlProcess,
    getNodeInfo
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)

import Utils (safeReceive, timeoutForCall)
import qualified Base.Broadcaster as B
import qualified Base.GlobalRegistry as GR
import qualified Base.GlobalCache as GC


data GetNodeInfo = GetNodeInfo deriving (Generic, Typeable)

instance Binary GetNodeInfo


data NodeInfo = NodeInfo {
        nodeId :: NodeId,
        stats :: NodeStats,
        broadcasterIsRunning :: Bool,
        globalRegistryIsRunning :: Bool,
        globalCacheIsRunning :: Bool
    }
    deriving (Generic, Typeable)

instance Binary NodeInfo


nodeControlProcess :: Process ()
nodeControlProcess = forever $ safeReceive handlers ()
    where
        prepareCall = callResponse
        handlers = [
            prepareCall handleGetNodeInfo,
            matchUnknown (return ())
            ]


handleGetNodeInfo :: GetNodeInfo -> Process (NodeInfo, ())
handleGetNodeInfo GetNodeInfo = do
    info <-
        NodeInfo <$>
        (processNodeId <$> getSelfPid) <*>
        getLocalNodeStats <*>
        B.isRunning <*>
        GR.isRunning <*>
        GC.isRunning
    return (info, ())


--external interface

getNodeInfo :: ProcessId -> TagPool -> Process NodeInfo
getNodeInfo pid tagPool = do
    tag <- getTag tagPool
    Just res <- callTimeout pid GetNodeInfo tag timeoutForCall
    return res



