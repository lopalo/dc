{-# LANGUAGE OverloadedStrings #-}

module Admin.API (apiHandlers) where

import Data.Aeson (object, (.=))
import Control.Distributed.Process (NodeStats(..))
import Control.Distributed.Process.Node (LocalNode)
import Web.Scotty hiding (settings)

import Admin.Common (getGlobalNames, killProcessByName, getNodeStatistic)

apiHandlers :: LocalNode -> ScottyM ()
apiHandlers node = do
    g "registry" $ getGlobalNames node >>= json
    p "kill-process-by-name" $ killProcessByName node
    g "node-stats" $ getNodeStats node
    where g = get . prfx
          p = post . prfx
          prfx = capture . ("/api/" ++)

getNodeStats :: LocalNode -> ActionM ()
getNodeStats node = do
    stats <- getNodeStatistic node
    json $ object ["node-id" .= show (nodeStatsNode stats),
                   "registered-names" .= nodeStatsRegisteredNames stats,
                   "monitors" .= nodeStatsMonitors stats,
                   "links" .= nodeStatsLinks stats,
                   "processes" .= nodeStatsProcesses stats]


--TOOD: rename module
