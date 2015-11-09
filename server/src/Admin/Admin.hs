{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin (adminServer) where


import Data.List (stripPrefix)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Network.Wai.Middleware.Static (staticPolicy, addBase, policy, (>->))
import Web.Scotty hiding (settings)

import qualified Admin.Settings as S
import Admin.API (apiHandlers)
import Admin.UI (uiHandlers, initHeistState)


adminServer :: S.Settings -> LocalNode -> Process ()
adminServer settings node = liftIO $ do
    let staticPrefix = policy (stripPrefix "static/")
        staticDir = S.staticDir settings
    heist <- initHeistState settings
    scotty (S.port settings) $ do
        middleware $ staticPolicy (staticPrefix >-> addBase staticDir)
        apiHandlers node
        uiHandlers node heist
        get "/" $ redirect "/ui"
        --TODO: replace 'static' with 'ui', similar to HTTPServer.hs







