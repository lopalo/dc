{-# LANGUAGE OverloadedStrings #-}

module HTTP.HTTP (httpProcess) where

import Data.List (stripPrefix)
import Data.String.Utils (split)
import Data.Text.Lazy (unpack)
import Data.String (fromString)

import Control.Distributed.Process (Process, liftIO)
import Control.Distributed.Process.Node (LocalNode)
import Control.Distributed.Process.Extras (newTagPool)
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Static (
    CachingStrategy(PublicStaticCaching, NoCaching),
    initCaching,
    staticPolicy', addBase, only, policy, (<|>), (>->)
    )
import Web.Scotty hiding (settings)

import qualified Base.GlobalCache as GC
import qualified HTTP.Settings as HS
import Utils (execProcess)


httpProcess :: HS.Settings -> LocalNode -> String -> Int -> Process ()
httpProcess settings node host port  =
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
            customPolicy =
                only [
                    ("", clientDir ++ "/index.html"),
                    ("client/js/settings.json", HS.clientSettings settings)
                    ]
            clientPolicy = clientPrefix >-> addBase clientDir
            staticMiddleware =
                staticPolicy' cacheContainer $ customPolicy <|> clientPolicy
        scottyOpts scottyOptions $ do
            middleware staticMiddleware
            get "/ws-addresses" $ do
                wsAddresses <-
                    execProcess node $ GC.get GC.WSAddressTag =<< newTagPool
                Just hostHeader <- header "Host"
                let hostName = head $ split ":" $ unpack hostHeader
                    f (name, GC.WSAddress h p) =
                        if h == "<http-host>"
                            then (name, hostName, p)
                            else (name, h, p)
                json $ map f wsAddresses



