{-# LANGUAGE OverloadedStrings #-}

module Service (spawnService) where

import Prelude hiding (log)
import Control.Monad (forever, when, void, mzero)
import Control.Monad.Catch (throwM)
import Data.Maybe (isNothing)
import System.Random (randomRIO)
import Control.Exception (SomeException)

import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types (
    ProcessExitException(..),
    messageFingerprint
    )
import Control.Distributed.Process.Serializable (fingerprint)
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Time (milliSeconds)
import Control.Distributed.Process.Extras.Timer (sleep)
import qualified Control.Distributed.Process.Node as Node
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Aeson.Types (parse)

import Types (
    NodeName, LogLevel(..), ServiceType(..),
    SwitchOffService(..), prefix
    )
import qualified Base.Logger as L
import Base.GlobalRegistry (globalRegister, globalWhereIs)
import Area.Area (areaProcess)
import DB.AreaDB (areaDBProcess)
import DB.UserDB (userDBProcess)
import HTTP.HTTP (httpProcess)
import WS.WS (wsProcess)
import Admin.Admin (adminProcess)
import NodeAgent.NodeAgent (nodeAgentProcess, distributedWhereIs)
import LogAggregator.LogAggregator (logAggregatorProcess)
import qualified Settings as S


spawnService ::
    NodeName -> Node.LocalNode -> S.Settings ->
    TagPool -> S.ServiceSettings -> Process ()
spawnService nodeName node settings tagPool serviceSettings = do
    tag <- getTag tagPool
    let delayR = S.respawnDelayMilliseconds $ S.cluster settings
        randomDelay = liftIO $ randomRIO delayR
        serviceType = S.serviceType serviceSettings
        ident = S.ident serviceSettings
        options = S.options serviceSettings
        name = prefix serviceType ++ ident
        uniqueName = concat ["service:", nodeName, ":", name, ":", show tag]

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

        wait delayFactor = do
            (delayFactor *) <$> randomDelay >>= sleep . milliSeconds
        serviceLoop service delayFactor = do
            pid <- getSelfPid
            globalRegister uniqueName pid tagPool
            wait delayFactor
            forever $ do
                --log Debug $ "Try to start: " ++ name
                globalRegister uniqueName pid tagPool
                maybePid <- globalWhereIs name tagPool --optimization
                when (isNothing maybePid) $ do
                    servicePids <- distributedWhereIs name tagPool
                    --reduces probability of conflicts during disconnection
                    when (null servicePids) $ do
                        ok <- globalRegister name pid tagPool
                        when ok $ do
                            log Info $ "Start: " ++ name
                            service
                wait 1
        spawnLoop service delayFactor =
            void $ spawnLocal $ spawning `respawnService` sequel
            where
                spawning = serviceLoop service (delayFactor :: Int)
                sequel = spawnLoop service $ delayFactor + 1
    case parse (initService serviceType) options of
        Aeson.Success service -> void $ spawnLoop service 0
        Aeson.Error err -> log Error $ "Init error: " ++ err


respawnService :: Process () -> Process () -> Process ()
spawning `respawnService` sequel = do
    spawning `catches` [Handler handleExit, Handler handleAny]
    sequel
    where
        handleExit ex@(ProcessExitException _ msg) =
            if messageFingerprint msg == fingerprint SwitchOffService
                then throwM ex
                else sequel >> throwM ex

        handleAny :: SomeException -> Process a
        handleAny ex = sequel >> throwM ex



log :: LogLevel -> String -> Process ()
log level txt = L.log level $ "Service - " ++ txt

