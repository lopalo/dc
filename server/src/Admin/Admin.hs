{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin (adminProcess) where

import Data.List (stripPrefix)
import qualified Data.Map.Strict as M

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Network.Wai.Middleware.Static (staticPolicy, addBase, policy, (>->))
import Web.Scotty hiding (settings)

import qualified Settings as S
import qualified Admin.Settings as AS
import Admin.ClusterHandlers (clusterHandlers)


adminProcess :: S.Settings -> LocalNode -> String -> Int -> Process ()
adminProcess settings node host port = liftIO $ do
    --TODO: use host
    let uiPrefix = policy (stripPrefix "ui/")
        uiDir = AS.uiDir $ S.admin settings
        nodeNames = S.nodeNames $ S.nodes settings
    scotty port $ do
        middleware $ staticPolicy (uiPrefix >-> addBase uiDir)
        clusterHandlers node nodeNames
        get "/" $ redirect "/ui/index.html"







