{-# LANGUAGE OverloadedStrings #-}

module HTTP.HTTP (httpProcess) where

import Data.List (stripPrefix)
import Data.String.Utils (split)
import Data.Text.Lazy (unpack)
import Data.String (fromString)

import Control.Distributed.Process (Process, liftIO)
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Static (
    CachingStrategy(PublicStaticCaching, NoCaching),
    initCaching,
    staticPolicy', addBase, only, policy, (<|>), (>->)
    )
import Web.Scotty hiding (settings)

import qualified HTTP.Settings as HS


httpProcess :: HS.Settings -> String -> Int -> Process ()
httpProcess settings host port  =
    liftIO $ do
        cacheContainer <-
            initCaching $
                if HS.caching settings
                    then PublicStaticCaching
                    else NoCaching
        let
            waiSettings = foldr ($) W.defaultSettings [
                W.setPort port,
                W.setHost (fromString host)
                ]
            scottyOptions = Options (HS.verbose settings) waiSettings
            clientPrefix = policy (stripPrefix "client/")
            clientDir = HS.clientDir settings
            clientPolicy = clientPrefix >-> addBase clientDir
            settingsPolicy =
                only [("client/js/settings.json", HS.clientSettings settings)]
            wsAddresses = HS.wsAddresses settings
            staticMiddleware =
                staticPolicy' cacheContainer $ settingsPolicy <|> clientPolicy
        scottyOpts scottyOptions $ do
            middleware staticMiddleware
            get "/ws-addresses" $ do
                Just hostHeader <- header "Host"
                let hostName = head $ split ":" $ unpack hostHeader
                    f (h, p) =
                        if h == "<http-host>"
                            then (hostName, p)
                            else (h, p)
                json $ map f wsAddresses
            get "/" $ redirect "/client/index.html"



