{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin (adminServer) where


import Data.List (stripPrefix)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Network.Wai.Middleware.Static (staticPolicy, addBase, policy, (>->))
import Web.Scotty hiding (settings)

import qualified Admin.Settings as S
import Admin.ClusterHandlers (clusterHandlers)


adminServer :: S.Settings -> LocalNode -> Process ()
adminServer settings node = liftIO $ do
    let uiPrefix = policy (stripPrefix "ui/")
        uiDir = S.uiDir settings
    scotty (S.port settings) $ do
        middleware $ staticPolicy (uiPrefix >-> addBase uiDir)
        clusterHandlers node
        get "/" $ redirect "/ui/index.html"







