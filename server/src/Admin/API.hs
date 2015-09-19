{-# LANGUAGE OverloadedStrings #-}

module Admin.API (apiHandlers) where

import Control.Distributed.Process.Node (LocalNode)
import Web.Scotty hiding (settings)

import Admin.Common (getGlobalNames, killProcessByName)

apiHandlers :: LocalNode -> ScottyM ()
apiHandlers node = do
    g "registry" $ getGlobalNames node >>= json
    p "kill-process-by-name" $ killProcessByName node
    --TODO: get node statistic (getNodeStats)
    where g = get . prfx
          p = post . prfx
          prfx = capture . ("/api/" ++)
