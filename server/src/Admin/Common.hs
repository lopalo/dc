{-# LANGUAGE OverloadedStrings #-}

module Admin.Common where

import qualified Data.Map.Strict as M
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode, runProcess)
import Control.Distributed.Process.Extras (newTagPool)
import Web.Scotty hiding (settings)


import GlobalRegistry (getRegistry)


type NameRecord = (String, String)


getGlobalNames :: LocalNode -> ActionM [NameRecord]
getGlobalNames node = do
    Just registry <- execProcess node $ getRegistry =<< newTagPool
    return [(name, show (processNodeId pid)) | (pid, name) <- M.toList registry]


killProcessByName :: LocalNode -> ActionM ()
killProcessByName node = do
    name <- param "name"
    execProcess node $ do
        Just pid <- whereis name
        kill pid "admin"


getNodeStatistic :: LocalNode -> ActionM NodeStats
getNodeStatistic node =
    execProcess node getLocalNodeStats


execProcess :: LocalNode -> Process a -> ActionM a
execProcess node proc = liftIO $ do
    var <- newEmptyMVar
    runProcess node $ do
        res <- proc
        liftIO $ putMVar var res
    takeMVar var

