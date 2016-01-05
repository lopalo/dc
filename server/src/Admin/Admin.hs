{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin (adminProcess) where


import Data.List (stripPrefix)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Network.Wai.Middleware.Static (staticPolicy, addBase, policy, (>->))
import Web.Scotty hiding (settings)

import qualified Admin.Settings as S
import Admin.ClusterHandlers (clusterHandlers)


adminProcess :: S.Settings -> LocalNode -> String -> Int -> Process ()
adminProcess settings node host port = liftIO $ do
    --TODO: use host
    let uiPrefix = policy (stripPrefix "ui/")
        uiDir = S.uiDir settings
    scotty port $ do
        middleware $ staticPolicy (uiPrefix >-> addBase uiDir)
        clusterHandlers node
        get "/" $ redirect "/ui/index.html"







