{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module NodeAgent.NodeAgent (
    GetNodeStatus(..), NodeStatus(..),
    nodeAgentProcess,
    getNodeStatus,
    distributedWhereIs
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, newTagPool, getTag)
import Control.Distributed.Process.Extras.Call (
    callResponse, callTimeout, multicall
    )

import Types (nodePrefix)
import Utils (safeReceive, timeoutForCall)
import qualified Base.GlobalRegistry as GR
import qualified Base.Logger as L


data GetNodeStatus = GetNodeStatus deriving (Generic, Typeable)

instance Binary GetNodeStatus


data WhereIs = WhereIs String deriving (Generic, Typeable)

instance Binary WhereIs


data NodeStatus = NodeStatus {
        stats :: NodeStats,
        globalRegistryIsRunning :: Bool,
        loggerIsRunning :: Bool
    }
    deriving (Generic, Typeable)

instance Binary NodeStatus


nodeAgentProcess :: Process ()
nodeAgentProcess = do
    tagPool <- newTagPool
    let prepareCall h = callResponse (h tagPool)
        handlers = [
            prepareCall handleWhereIs,
            prepareCall handleGetNodeStatus,
            matchUnknown (return ())
            ]
    forever $ safeReceive handlers ()


handleWhereIs :: TagPool -> WhereIs -> Process (Maybe ProcessId, ())
handleWhereIs tagPool (WhereIs name) = do
    maybePid <- GR.globalWhereIs name tagPool
    return (maybePid, ())


handleGetNodeStatus :: TagPool -> GetNodeStatus -> Process (NodeStatus, ())
handleGetNodeStatus _ GetNodeStatus = do
    info <-
        NodeStatus <$>
        getLocalNodeStats <*>
        GR.isRunning <*>
        L.isRunning
    return (info, ())


distributedRequest ::
    (Serializable a, Serializable b) => a -> TagPool -> Process [b]
distributedRequest msg tagPool = do
    nodeAgentPids <- GR.globalWhereIsByPrefix nodePrefix tagPool
    tag <- getTag tagPool
    res <- multicall nodeAgentPids msg tag timeoutForCall
    return [val | Just val <- res]


--external interface


getNodeStatus :: TagPool -> Process [NodeStatus]
getNodeStatus = distributedRequest GetNodeStatus


distributedWhereIs :: String -> TagPool -> Process [ProcessId]
distributedWhereIs name tagPool = do
    res <- distributedRequest (WhereIs name) tagPool
    return [pid | Just pid <- res]
