{-# LANGUAGE OverloadedStrings #-}

module Admin.ClusterHandlers (clusterHandlers) where

import qualified Data.Map.Strict as M

import Data.Aeson (object, (.=))
import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Control.Distributed.Process.Extras (newTagPool)
import Web.Scotty hiding (settings)

import Types (NodeName, NodeNames)
import Utils (milliseconds)
import Admin.Utils (execProcess)
import qualified Base.GlobalRegistry as GR


clusterHandlers :: LocalNode -> NodeNames -> ScottyM ()
clusterHandlers node nodeNames = do
    g "registry" $ getRegistry node nodeNames
    p "kill-process-by-name" $ killProcessByName node
    g "node-stats" $ getNodeStatistic node
    where
        g = get . prfx
        p = post . prfx
        prfx = capture . ("/cluster/" ++)


getNodeStatistic :: LocalNode -> ActionM ()
getNodeStatistic node = do
    stats <- execProcess node getLocalNodeStats
    --TODO: use getNodeInfo for each of nodes that defined in settings;
    --      get pids of node controls by globalMultiWhereIs
    json $ object [
        "node-id" .= show (nodeStatsNode stats),
        "registered-names" .= nodeStatsRegisteredNames stats,
        "monitors" .= nodeStatsMonitors stats,
        "links" .= nodeStatsLinks stats,
        "processes" .= nodeStatsProcesses stats
        ]



getRegistry :: LocalNode -> NodeNames -> ActionM ()
getRegistry node nodeNames = do
    registry <- execProcess node $ GR.getNameList "" =<< newTagPool
    now <- liftIO milliseconds
    let nodeName = (nodeNames M.!) . processNodeId
        uptime ts = show $ now - ts --TODO: format string
    json [(name, nodeName pid, uptime ts) | (name, pid, ts) <- registry]



killProcessByName :: LocalNode -> ActionM ()
killProcessByName node = do
    name <- param "name"
    execProcess node $ do
        Just pid <- GR.globalWhereIs name =<< newTagPool
        kill pid "admin"

