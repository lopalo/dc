{-# LANGUAGE OverloadedStrings #-}

module Service (spawnService) where

import Prelude hiding (log)
import Control.Monad (forever, when, void, mzero)
import Data.Maybe (isNothing)
import Data.String.Utils (endswith)
import System.Random (randomRIO)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool)
import Control.Distributed.Process.Extras.Time (milliSeconds)
import Control.Distributed.Process.Extras.Timer (sleep)
import qualified Control.Distributed.Process.Node as Node
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Aeson.Types (parse)

import Types (NodeName, LogLevel(..), ServiceType(..), prefix)
import qualified Base.Logger as L
import Base.GlobalRegistry (globalRegister, globalWhereIs)
import Area.Area (areaProcess)
import DB.AreaDB (areaDBProcess)
import DB.UserDB (userDBProcess)
import HTTP.HTTP (httpProcess)
import WS.WS (wsProcess)
import Admin.Admin (adminProcess)
import NodeAgent.NodeAgent (nodeAgentProcess)
import LogAggregator.LogAggregator (logAggregatorProcess)
import qualified Settings as S


spawnService ::
    NodeName -> Node.LocalNode -> S.Settings ->
    TagPool -> S.ServiceSettings -> Process ()
spawnService nodeName node settings tagPool serviceSettings = do
    let delayR = S.respawnDelayMilliseconds $ S.cluster settings
        randomDelay = liftIO $ randomRIO delayR
        serviceType = S.serviceType serviceSettings
        ident = S.ident serviceSettings
        options = S.options serviceSettings
        name = prefix serviceType ++ ident

        initService AreaDB _ =
            return $ areaDBProcess (S.db settings) ident
        initService UserDB _ =
            return $ userDBProcess (S.db settings) ident
        initService WS opts = do
            host <- opts .: "host"
            port <- opts .: "port"
            return $ wsProcess settings node ident host port
        initService HTTP opts = do
            host <- opts .: "host"
            port <- opts .: "port"
            return $ httpProcess (S.http settings) node host port
        initService Admin opts = do
            host <- opts .: "host"
            port <- opts .: "port"
            return $ adminProcess settings node host port
        initService Area opts = do
            minReplicas <- opts .: "min-db-replicas"
            return $ areaProcess (S.area settings) ident minReplicas
        initService NodeAgent _ =
            let spawner = spawnService nodeName node settings tagPool
            in return $ nodeAgentProcess spawner
        initService LogAggregator _ =
            return logAggregatorProcess
        initService _ _ = mzero

        wait delayFactor =
            (delayFactor *) <$> randomDelay >>= sleep . milliSeconds

        service process supervisorPid = do
            link supervisorPid
            pid <- getSelfPid
            forever $ do
                --log Debug $ "Try to start: " ++ name
                maybePid <- globalWhereIs name tagPool
                when (isNothing maybePid) $ do
                    globalRegister name pid tagPool
                    log Info $ "Start: " ++ name
                    process
                wait 1

        supervisor service' = do
            pid <- getSelfPid
            let
                loop delayFactor = do
                    wait delayFactor
                    spawnLocal (service' pid) >>= monitor
                    ProcessMonitorNotification _ _ reason <-
                        expect :: Process ProcessMonitorNotification
                    case reason of
                        DiedException str
                            | endswith "switch-off" str -> return ()
                        _ -> loop $ delayFactor + 1
            loop 0

    case parse (initService serviceType) options of
        Aeson.Success process ->
            void $ spawnLocal $ supervisor $ service process
        Aeson.Error err -> log Error $ "Init error: " ++ err


log :: LogLevel -> String -> Process ()
log level txt = L.log level $ "Service - " ++ txt

