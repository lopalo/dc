{-# LANGUAGE OverloadedStrings #-}

module App.HTTPServer (httpServer) where

import Control.Distributed.Process (Process, liftIO)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))
import Web.Scotty hiding (settings)

import qualified App.Settings as S


httpServer :: S.Settings -> Process ()
httpServer settings = liftIO $ do
    let clientDir = S.clientDir settings
        wsAddresses = json $ S.wsAddresses settings
    scotty (S.httpPort settings) $ do
        middleware $ staticPolicy (noDots >-> addBase clientDir)
        get "/ws-addresses" wsAddresses
        get "/" $ redirect "/index.html"



