{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module NodeAgent.NodeAgent (
    GetNodeStatus(..), NodeStatus(..),
    nodeAgentProcess,
    getNodeStatus,
    getClusterMesh,
    distributedWhereIs
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)
import qualified Data.Map.Strict as M

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, newTagPool)
import Control.Distributed.Process.Extras.Call (callResponse)

import Types (NodeName, ServiceType(NodeAgent), prefix)
import Utils (safeReceive)
import qualified Base.Broadcaster as B
import qualified Base.GlobalRegistry as GR
import qualified Base.Logger as L


data GetNodeStatus = GetNodeStatus deriving (Generic, Typeable)

instance Binary GetNodeStatus


data GetClusterMesh = GetClusterMesh deriving (Generic, Typeable)

instance Binary GetClusterMesh


data WhereIs = WhereIs String deriving (Generic, Typeable)

instance Binary WhereIs


data NodeStatus = NodeStatus {
        stats :: NodeStats,
        broadcasterIsRunning :: Bool,
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
            prepareCall handleGetClusterMesh,
            matchUnknown (return ())
            ]
    forever $ safeReceive handlers ()


handleWhereIs :: TagPool -> WhereIs -> Process (Maybe ProcessId, ())
handleWhereIs tagPool (WhereIs name) = do
    maybePid <- GR.globalWhereIs name tagPool
    return (maybePid, ())


handleGetNodeStatus :: TagPool -> GetNodeStatus -> Process (NodeStatus, ())
handleGetNodeStatus _ _ = do
    info <-
        NodeStatus <$>
        getLocalNodeStats <*>
        B.isRunning <*>
        GR.isRunning <*>
        L.isRunning
    return (info, ())


handleGetClusterMesh ::
    TagPool -> GetClusterMesh -> Process ((NodeName, M.Map NodeName Bool), ())
handleGetClusterMesh tagPool _ = do
    res <- GR.getVisibleNodes tagPool
    return (res, ())


distributedRequest ::
    (Serializable a, Serializable b) => a -> TagPool -> Process [b]
distributedRequest = GR.multicallByPrefix $ prefix NodeAgent


--external interface


getNodeStatus :: TagPool -> Process [NodeStatus]
getNodeStatus = distributedRequest GetNodeStatus


getClusterMesh :: TagPool -> Process (M.Map NodeName (M.Map NodeName Bool))
getClusterMesh tagPool =
    M.fromList <$> distributedRequest GetClusterMesh tagPool


distributedWhereIs :: String -> TagPool -> Process [ProcessId]
distributedWhereIs name tagPool = do
    res <- distributedRequest (WhereIs name) tagPool
    return [pid | Just pid <- res]


