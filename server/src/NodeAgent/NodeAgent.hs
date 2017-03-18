{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module NodeAgent.NodeAgent (
    GetNodeStatus(..), NodeStatus(..),
    nodeAgentProcess,
    getNodeStatus,
    getClusterMesh,
    startService
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
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
import qualified Base.GlobalCache as GC
import qualified Base.Logger as L
import qualified Settings as S


data GetNodeStatus = GetNodeStatus deriving (Generic, Typeable)

instance Binary GetNodeStatus


data GetClusterMesh = GetClusterMesh deriving (Generic, Typeable)

instance Binary GetClusterMesh


data StartService =
    StartService S.ServiceSettings
    deriving (Generic, Typeable)

instance Binary StartService


data NodeStatus = NodeStatus {
        stats :: NodeStats,
        broadcasterIsRunning :: Bool,
        globalRegistryIsRunning :: Bool,
        globalCacheIsRunning :: Bool,
        loggerIsRunning :: Bool
    }
    deriving (Generic, Typeable)

instance Binary NodeStatus


type State = (TagPool, S.ServiceSettings -> Process ())


nodeAgentProcess :: (S.ServiceSettings -> Process ()) -> Process ()
nodeAgentProcess spawnService = do
    tagPool <- newTagPool
    let state = (tagPool, spawnService)
        prepare h = match (h state)
        prepareCall h = callResponse (h state)
        handlers = [
            prepareCall handleGetNodeStatus,
            prepareCall handleGetClusterMesh,
            prepare handleStartService,
            matchUnknown (return ())
            ]
    forever $ safeReceive handlers ()


handleGetNodeStatus :: State -> GetNodeStatus -> Process (NodeStatus, ())
handleGetNodeStatus _ _ = do
    info <-
        NodeStatus <$>
        getLocalNodeStats <*>
        B.isRunning <*>
        GR.isRunning <*>
        GC.isRunning <*>
        L.isRunning
    return (info, ())


handleGetClusterMesh ::
    State -> GetClusterMesh -> Process ((NodeName, M.Map NodeName Bool), ())
handleGetClusterMesh (tagPool, _) _ = do
    res <- GR.getVisibleNodes tagPool
    return (res, ())


handleStartService :: State -> StartService -> Process ()
handleStartService (_, spawnService) (StartService serviceSettings) =
    spawnService serviceSettings


distributedRequest ::
    (Serializable a, Serializable b) => a -> TagPool -> Process [b]
distributedRequest = GR.multicallByPrefix $ prefix NodeAgent


--external interface


getNodeStatus :: TagPool -> Process [NodeStatus]
getNodeStatus = distributedRequest GetNodeStatus


getClusterMesh :: TagPool -> Process (M.Map NodeName (M.Map NodeName Bool))
getClusterMesh tagPool =
    M.fromList <$> distributedRequest GetClusterMesh tagPool


startService :: NodeName -> S.ServiceSettings -> Process ()
startService nodeName serviceSettings =
    GR.globalNSend name $ StartService serviceSettings
    where name = prefix NodeAgent ++ nodeName
