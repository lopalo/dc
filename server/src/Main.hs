{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)
import Control.Monad (forever, when, void, mzero)
import Data.Maybe (isNothing)
import System.Environment (getArgs)
import Data.Map.Strict ((!))
import System.Random (randomRIO)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, newTagPool)
import Control.Distributed.Process.Extras.Time (milliSeconds)
import Control.Distributed.Process.Extras.Timer (sleep)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Control.Distributed.Process.Node as Node
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.:))
import Data.Aeson.Types (parse)
import Data.Yaml (ParseException, decodeFileEither)

import Utils (sleepSeconds)
import Types (NodeName, LogLevel(..), ServiceType(..), prefix)
import Base.Broadcaster (broadcasterProcess)
import Base.Logger (loggerProcess, log)
import Base.GlobalRegistry (
    globalRegistryProcess,
    globalRegister,
    globalWhereIs
    )
import Area.Area (areaProcess)
import DB.AreaDB (areaDBProcess)
import DB.UserDB (userDBProcess)
import HTTP.HTTP (httpProcess)
import WS.WS (wsProcess)
import WS.Connection (connectionBroadcastHandlers)
import User.External (userBroadcastHandlers)
import Admin.Admin (adminProcess)
import NodeAgent.NodeAgent (nodeAgentProcess, distributedWhereIs)
import LogAggregator.LogAggregator (logAggregatorProcess, createSender)
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


spawnService ::
    Node.LocalNode -> S.Settings -> TagPool -> S.ServiceSettings -> Process ()
spawnService node settings tagPool serviceSettings = do
    let delayR = S.respawnDelayMilliseconds $ S.cluster settings
        randomDelay = liftIO $ randomRIO delayR
        serviceType = S.serviceType serviceSettings
        ident = S.ident serviceSettings
        options = S.options serviceSettings
        name = prefix serviceType ++ ident

        initService AreaDB opts =
            return $ areaDBProcess (S.db settings) ident
        initService UserDB _ =
            return $ userDBProcess (S.db settings) ident
        initService WS opts = do
            host <- opts .: "host"
            port <- opts .: "port"
            return $ wsProcess settings node host port
        initService HTTP opts = do
            host <- opts .: "host"
            port <- opts .: "port"
            return $ httpProcess (S.http settings) host port
        initService Admin opts = do
            host <- opts .: "host"
            port <- opts .: "port"
            return $ adminProcess settings node host port
        initService Area opts = do
            minReplicas <- opts .: "min-db-replicas"
            return $ areaProcess (S.area settings) ident minReplicas
        initService NodeAgent _ =
            return nodeAgentProcess
        initService LogAggregator _ =
            return logAggregatorProcess
        initService _ _ = mzero

        wait = randomDelay >>= sleep . milliSeconds
        serviceLoop service = do
            wait
            forever $ do
                --log Debug $ "Try to start service: " ++ name
                maybePid <- globalWhereIs name tagPool --optimization
                when (isNothing maybePid) $ do
                    servicePids <- distributedWhereIs name tagPool
                    --reduces probability of conflicts during disconnection
                    when (null servicePids) $ do
                        pid <- getSelfPid
                        ok <- globalRegister name pid tagPool
                        when ok $ do
                            log Info $ "Start service: " ++ name
                            service
                wait
        spawnLoop service =
            spawnLocal $ serviceLoop service `finally` spawnLoop service
    case parse (initService serviceType) options of
        Aeson.Success service -> void $ spawnLoop service
        Aeson.Error err -> log Error $ "Service init error: " ++ err


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

