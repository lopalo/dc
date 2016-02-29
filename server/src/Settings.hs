{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Aeson.Types (Parser)
import Control.Distributed.Process
import Network.Transport (EndPointAddress(EndPointAddress))

import Types (NodeName, NodeNames, LogLevel)
import qualified Area.Settings
import qualified User.Settings
import qualified HTTP.Settings
import qualified Admin.Settings


data Settings = Settings {
    log :: LogSettings,
    cluster :: ClusterSettings,
    user :: User.Settings.Settings,
    area :: Area.Settings.Settings,
    http :: HTTP.Settings.Settings,
    admin :: Admin.Settings.Settings,
    nodes :: M.Map NodeName NodeSettings
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "log" <*>
        v .: "cluster" <*>
        v .: "user" <*>
        v .: "area" <*>
        v .: "http" <*>
        v .: "admin" <*>
        v .: "nodes"
    parseJSON _ = mzero


data LogSettings = LogSettings {
    logLevel :: LogLevel,
    stdout :: Bool,
    logFile :: Maybe String,
    logAggregatorName :: Maybe String
    }

instance FromJSON LogSettings where

    parseJSON (Object v) =
        LogSettings <$>
        v .: "log-level" <*>
        v .: "stdout" <*>
        v .: "logFile" <*>
        v .: "aggregator-name"
    parseJSON _ = mzero



data ClusterSettings = ClusterSettings {
    respawnDelayMilliseconds :: (Int, Int),
    nodePingPeriodMilliseconds :: Int
    }

instance FromJSON ClusterSettings where

    parseJSON (Object v) =
        ClusterSettings <$>
        v .: "respawn-delay-milliseconds" <*>
        v .: "node-ping-period-milliseconds"
    parseJSON _ = mzero


data NodeSettings = NodeSettings {
    nodeHost :: String,
    nodePort :: Int,
    services :: [ServiceSettings]
    }

instance FromJSON NodeSettings where

    parseJSON (Object v) =
        NodeSettings <$>
        v .: "node-host" <*>
        v .: "node-port" <*>
        v .: "services"
    parseJSON _ = mzero


data ServiceSettings
    = DB {ident :: String, path :: String}
    | WS {ident :: String, host :: String, port :: Int}
    | HTTP {ident :: String, host :: String, port :: Int}
    | Admin {ident :: String, host :: String, port :: Int}
    | Area {ident :: String}
    | NodeAgent {ident :: String}
    | LogAggregator {ident :: String}

instance FromJSON ServiceSettings where

    parseJSON (Object v) = do
        sType <- v .: "type" :: Parser String
        i <- v .: "ident" :: Parser String
        case sType of
            "db" -> DB i <$> v .: "path"
            "ws" -> WS i <$> v .: "host" <*> v .: "port"
            "http" -> HTTP i <$> v .: "host" <*> v .: "port"
            "admin" -> Admin i <$> v .: "host" <*> v .: "port"
            "area" -> return $ Area i
            "log-aggregator" -> return $ LogAggregator i
            _ -> mzero
    parseJSON _ = mzero


makeNodeId :: String -> Int -> NodeId
makeNodeId nHost nPort =
    let addr = nHost ++ ":" ++ show nPort
    in NodeId . EndPointAddress . B.concat $ [B.pack addr, ":0"]


nodeId :: NodeSettings -> NodeId
nodeId s = makeNodeId (nodeHost s) (nodePort s)


nodeNames :: M.Map NodeName NodeSettings -> NodeNames
nodeNames = M.foldMapWithKey f
    where f nodeName ss = M.singleton (nodeId ss) nodeName


