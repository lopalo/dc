{-# LANGUAGE OverloadedStrings #-}

module Admin.ClusterHandlers (clusterHandlers) where

import Data.Tuple.Utils (snd3)
import qualified Data.Map.Strict as M

import Data.Aeson (object, (.=))
import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Control.Distributed.Process.Extras (newTagPool)
import Web.Scotty hiding (settings, status)

import Types (NodeNames)
import Utils (milliseconds)
import Admin.Utils (execProcess)
import qualified Base.GlobalRegistry as GR
import qualified NodeAgent.NodeAgent as NA


clusterHandlers :: LocalNode -> NodeNames -> ScottyM ()
clusterHandlers node nodeNames = do
    g "registry" $ getRegistry node nodeNames
    p "kill-process-by-name" $ killProcessByName node
    g "node-status" $ getNodeStatus node nodeNames
    where
        g = get . prfx
        p = post . prfx
        prfx = capture . ("/cluster/" ++)


getNodeStatus :: LocalNode -> NodeNames -> ActionM ()
getNodeStatus node nodeNames = do
    statusList <- execProcess node $ NA.getNodeStatus =<< newTagPool
    let res'' = foldl insert res statusList
        insert res' status =
            let nodeId = nodeStatsNode $ NA.stats status
                name = nodeNames M.! nodeId
                view = statusView status
            in M.insert name (Just view) res'
    json res''
    where
        res = foldl insertNothing M.empty $ M.elems nodeNames
        insertNothing res' name = M.insert name Nothing res'
        statusView status =
            object [
                "stats" .= statsView (NA.stats status),
                "broadcaster" .= NA.broadcasterIsRunning status,
                "global-registry" .= NA.globalRegistryIsRunning status,
                "logger" .= NA.loggerIsRunning status
                ]
        statsView stats =
            object [
                "node-id" .= show (nodeStatsNode stats),
                "registered-names" .= nodeStatsRegisteredNames stats,
                "monitors" .= nodeStatsMonitors stats,
                "links" .= nodeStatsLinks stats,
                "processes" .= nodeStatsProcesses stats
                ]



getRegistry :: LocalNode -> NodeNames -> ActionM ()
getRegistry node nodeNames = do
    namePrefix <- param "prefix" `rescue` \_ -> return ""
    nodeSelector <- param "node" `rescue` \_ -> return ""
    registry <- execProcess node $ GR.getNameList namePrefix =<< newTagPool
    now <- liftIO milliseconds
    let nodeName = (nodeNames M.!) . processNodeId
        uptime ts = show $ now - ts
        nodeFilter = (nodeSelector ==) . snd3
        res = [(name, nodeName pid, uptime ts) | (name, pid, ts) <- registry]
        res' = if null nodeSelector then res else filter nodeFilter res
    json res'



killProcessByName :: LocalNode -> ActionM ()
killProcessByName node = do
    name <- param "name"
    execProcess node $ do
        Just pid <- GR.globalWhereIs name =<< newTagPool
        kill pid "admin"

