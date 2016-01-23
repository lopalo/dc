{-# LANGUAGE OverloadedStrings #-}

module Admin.Utils where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode, runProcess)
import Web.Scotty hiding (settings)


execProcess :: LocalNode -> Process a -> ActionM a
execProcess node proc =
    liftIO $ do
        var <- newEmptyMVar
        runProcess node $ do
            res <- proc
            liftIO $ putMVar var res
        takeMVar var

