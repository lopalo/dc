{-# LANGUAGE OverloadedStrings #-}

module App.HTTPServer (httpServer) where

import Data.String.Utils (split)
import Data.Text.Lazy (unpack)

import Control.Distributed.Process (Process, liftIO)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty hiding (settings)

import qualified App.Settings as S


httpServer :: S.Settings -> Process ()
httpServer settings = liftIO $ do
    --TODO: use a cache container for the static middleware
    let clientDir = S.clientDir settings
        wsAddresses = S.wsAddresses settings
    scotty (S.httpPort settings) $ do
        middleware $ staticPolicy $ addBase clientDir
        get "/ws-addresses" $ do
            Just host <- header "Host"
            let hostName = head $ split ":" $ unpack host
                f (h, p) = if h == "<host>" then (hostName, p) else (h, p)
            json $ map f wsAddresses
        get "/" $ redirect "/index.html"



