{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin (adminServer) where

import Control.Distributed.Process.Extras (newTagPool)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)

import Control.Distributed.Process (Process, liftIO, whereis, kill)
import Control.Distributed.Process.Node (LocalNode, runProcess)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty hiding (settings)

import App.GlobalRegistry (getAllNames)
import qualified Admin.Settings as S


adminServer :: S.Settings -> LocalNode -> Process ()
adminServer settings node = liftIO $ do
    let staticDir = S.staticDir settings
    scotty (S.port settings) $ do
        middleware $ staticPolicy (addBase staticDir)
        get "/api/global-names" $ getGlobalNames node
        post "/api/kill-process-by-name" $ killProcessByName node
        get "/ui" $ html "<h1>Admin UI</h2>" --TODO /ui/<ui handler>


getGlobalNames :: LocalNode -> ActionM ()
getGlobalNames node = do
    Just names <- execProcess node $ getAllNames =<< newTagPool
    json names


killProcessByName :: LocalNode -> ActionM ()
killProcessByName node = do
    name <- param "name"
    execProcess node $ do
        Just pid <- whereis name
        kill pid "admin"


execProcess :: LocalNode -> Process a -> ActionM a
execProcess node proc = liftIO $ do
    var <- newEmptyMVar
    runProcess node $ do
        res <- proc
        liftIO $ putMVar var res
    takeMVar var


