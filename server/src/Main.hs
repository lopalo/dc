{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)
import Control.Monad (forever)
import System.Environment (getArgs)
import Data.Map.Strict ((!))

import Control.Distributed.Process
import Control.Distributed.Process.Extras (newTagPool)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Control.Distributed.Process.Node as Node
import qualified Data.Aeson as Aeson
import Data.Aeson (object)
import Data.Yaml (ParseException, decodeFileEither)

import Utils (sleepSeconds)
import Types (NodeName, LogLevel(..), ServiceType(..))
import Service (spawnService)
import Base.Broadcaster (broadcasterProcess)
import Base.Logger (loggerProcess, log)
import Base.GlobalRegistry (globalRegistryProcess)
import WS.Connection (connectionBroadcastHandlers)
import User.External (userBroadcastHandlers)
import LogAggregator.LogAggregator (createSender)
import qualified Settings as S


startNode ::
    Node.LocalNode -> S.Settings -> NodeName -> Process ()
startNode node settings nodeName = do
    let services = S.services $  S.nodes settings ! nodeName
        Aeson.Object emptyOptions = object []
        nodeService = S.ServiceSettings NodeAgent nodeName emptyOptions
        services' = nodeService : services
        logSettings = S.log settings
        broadcastHandlers = [
            connectionBroadcastHandlers,
            userBroadcastHandlers
            ]
    logSender <- createSender (S.logAggregatorName logSettings) nodeName
    spawnLocal $ loggerProcess logSettings logSender
    log Info $ "Start node: " ++ nodeName
    tagPool <- newTagPool
    spawnLocal $ broadcasterProcess $ concat broadcastHandlers
    spawnLocal $ globalRegistryProcess settings
    mapM_ (spawnService node settings tagPool) services'


main :: IO ()
main = do
    [settingsPath, nodeName] <- getArgs
    res <-
        decodeFileEither settingsPath
        :: IO (Either ParseException S.Settings)
    case res of
        Left err -> print err
        Right settings -> do
            let nodeSettings = S.nodes settings ! nodeName
                host = S.nodeHost nodeSettings
                port = show $ S.nodePort nodeSettings
            Right transport <- createTransport host port defaultTCPParameters
            node <- Node.newLocalNode transport Node.initRemoteTable
            Node.runProcess node $ startNode node settings nodeName
            forever $ sleepSeconds 1

