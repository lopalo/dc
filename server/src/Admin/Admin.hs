{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin (adminProcess) where

import Data.List (stripPrefix)
import Data.String (fromString)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Static (staticPolicy, addBase, policy, (>->))
import Web.Scotty hiding (settings)

import qualified Settings as S
import qualified Admin.Settings as AS
import Admin.ClusterHandlers (clusterHandlers)
import Admin.AreaHandlers (areaHandlers)


adminProcess :: S.Settings -> LocalNode -> String -> Int -> Process ()
adminProcess settings node host port = liftIO $ do
    let aSettings = S.admin settings
        waiSettings = foldr ($) W.defaultSettings [
            W.setPort port,
            W.setHost (fromString host)
            ]
        scottyOptions = Options (AS.verbose aSettings) waiSettings
        uiPrefix = policy (stripPrefix "ui/")
        uiDir = AS.uiDir aSettings
        nodeNames = S.nodeNames $ S.nodes settings
    scottyOpts scottyOptions $ do
        middleware $ staticPolicy (uiPrefix >-> addBase uiDir)
        clusterHandlers node nodeNames
        areaHandlers node
        get "/" $ redirect "/ui/index.html"







