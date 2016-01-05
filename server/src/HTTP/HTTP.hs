{-# LANGUAGE OverloadedStrings #-}

module HTTP.HTTP (httpProcess) where

import Data.List (stripPrefix)
import Data.String.Utils (split)
import Data.Text.Lazy (unpack)

import Control.Distributed.Process (Process, liftIO)
import Network.Wai.Middleware.Static (
    staticPolicy, addBase, only, policy, (<|>), (>->)
    )
import Web.Scotty hiding (settings)

import qualified HTTP.Settings as HS


httpProcess :: HS.Settings -> String -> Int -> Process ()
httpProcess settings host port  =
    liftIO $ do
        --TODO: use a cache container for the static middleware
        --TODO: use host
        let clientPrefix = policy (stripPrefix "client/")
            clientDir = HS.clientDir settings
            clientPolicy = clientPrefix >-> addBase clientDir
            settingsPolicy =
                only [("client/js/settings.json", HS.clientSettings settings)]
            wsAddresses = HS.wsAddresses settings
        scotty port $ do
            middleware $ staticPolicy $ settingsPolicy <|> clientPolicy
            get "/ws-addresses" $ do
                Just hostHeader <- header "Host"
                let hostName = head $ split ":" $ unpack hostHeader
                    f (h, p) =
                        if h == "<http-host>"
                            then (hostName, p)
                            else (h, p)
                json $ map f wsAddresses
            get "/" $ redirect "/client/index.html"



