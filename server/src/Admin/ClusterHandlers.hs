{-# LANGUAGE OverloadedStrings #-}

module Admin.ClusterHandlers (clusterHandlers) where

import Data.Aeson (object, (.=))
import Control.Distributed.Process (NodeStats(..))
import Control.Distributed.Process.Node (LocalNode)
import Web.Scotty hiding (settings)

import Admin.Utils (getGlobalNames, killProcessByName, getNodeStatistic)

clusterHandlers :: LocalNode -> ScottyM ()
clusterHandlers node = do
    g "registry" $ getGlobalNames node >>= json
    p "kill-process-by-name" $ killProcessByName node
    g "node-stats" $ getNodeStats node
    where g = get . prfx
          p = post . prfx
          prfx = capture . ("/cluster/" ++)

getNodeStats :: LocalNode -> ActionM ()
getNodeStats node = do
    stats <- getNodeStatistic node
    json $ object ["node-id" .= show (nodeStatsNode stats),
                   "registered-names" .= nodeStatsRegisteredNames stats,
                   "monitors" .= nodeStatsMonitors stats,
                   "links" .= nodeStatsLinks stats,
                   "processes" .= nodeStatsProcesses stats]


--TOOD: rename module
