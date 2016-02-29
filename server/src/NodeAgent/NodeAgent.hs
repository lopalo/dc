{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module NodeAgent.NodeAgent (
    GetNodeInfo(..), NodeInfo(..),
    nodeAgentProcess,
    getNodeInfo,
    distributedWhereIs
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, newTagPool, getTag)
import Control.Distributed.Process.Extras.Call (
    callResponse, callTimeout, multicall
    )

import Types (nodePrefix)
import Utils (safeReceive, timeoutForCall)
import qualified Base.GlobalRegistry as GR
import qualified Base.GlobalCache as GC
import qualified Base.Logger as L


data GetNodeInfo = GetNodeInfo deriving (Generic, Typeable)

instance Binary GetNodeInfo


data WhereIs = WhereIs String deriving (Generic, Typeable)

instance Binary WhereIs


data NodeInfo = NodeInfo {
        nodeId :: NodeId,
        stats :: NodeStats,
        globalRegistryIsRunning :: Bool,
        globalCacheIsRunning :: Bool,
        loggerIsRunning :: Bool
    }
    deriving (Generic, Typeable)

instance Binary NodeInfo


nodeAgentProcess :: Process ()
nodeAgentProcess = do
    tagPool <- newTagPool
    let prepareCall h = callResponse (h tagPool)
        handlers = [
            prepareCall handleWhereIs,
            prepareCall handleGetNodeInfo,
            matchUnknown (return ())
            ]
    forever $ safeReceive handlers ()


handleWhereIs :: TagPool -> WhereIs -> Process (Maybe ProcessId, ())
handleWhereIs tagPool (WhereIs name) = do
    maybePid <- GR.globalWhereIs name tagPool
    return (maybePid, ())


handleGetNodeInfo :: TagPool -> GetNodeInfo -> Process (NodeInfo, ())
handleGetNodeInfo _ GetNodeInfo = do
    info <-
        NodeInfo <$>
        (processNodeId <$> getSelfPid) <*>
        getLocalNodeStats <*>
        GR.isRunning <*>
        GC.isRunning <*>
        L.isRunning
    return (info, ())


--external interface

getNodeInfo :: ProcessId -> TagPool -> Process NodeInfo
getNodeInfo pid tagPool = do
    tag <- getTag tagPool
    Just res <- callTimeout pid GetNodeInfo tag timeoutForCall
    return res


distributedWhereIs :: String -> TagPool -> Process [ProcessId]
distributedWhereIs name tagPool = do
    nodeAgentPids <- GR.globalWhereIsByPrefix nodePrefix tagPool
    tag <- getTag tagPool
    res <- multicall nodeAgentPids (WhereIs name) tag timeoutForCall
    return $ [pid | Just (Just pid) <- res]
