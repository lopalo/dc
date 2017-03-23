{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Data.Binary (Binary, put, get)
import Control.Monad (mzero)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B

import Data.Aeson (FromJSON(parseJSON), Object, Value(Object), (.:))
import Data.Aeson.Types (Parser)
import Control.Distributed.Process
import Network.Transport (EndPointAddress(EndPointAddress))

import Types (ServiceId, NodeName, NodeNames, LogLevel, ServiceType(..))
import qualified DB.Settings
import qualified Area.Settings
import qualified User.Settings
import qualified HTTP.Settings
import qualified Admin.Settings


data Settings = Settings {
    log :: LogSettings,
    cluster :: ClusterSettings,
    db :: DB.Settings.Settings,
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
        v .: "db" <*>
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
    nodePingPeriodMilliseconds :: Int,
    tcp :: TCPSettings
    }

instance FromJSON ClusterSettings where

    parseJSON (Object v) =
        ClusterSettings <$>
        v .: "respawn-delay-milliseconds" <*>
        v .: "node-ping-period-milliseconds" <*>
        v .: "tcp"
    parseJSON _ = mzero


data TCPSettings = TCPSettings {
    connectTimeoutMicroseconds :: Maybe Int,
    noDelay :: Bool,
    userTimeoutMilliseconds :: Maybe Int
    }

instance FromJSON TCPSettings where

    parseJSON (Object v) =
        TCPSettings <$>
        v .: "connect-timeout-microseconds" <*>
        v .: "no-delay" <*>
        v .: "user-timeout-milliseconds"
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


data ServiceSettings = ServiceSettings {
    serviceType :: ServiceType,
    ident :: ServiceId,
    options :: Object
    }
    deriving (Show, Read)


instance Binary ServiceSettings where

    put t = put $ show t

    get = read <$> get


instance FromJSON ServiceSettings where

    parseJSON (Object v) = do
        typeName <- v .: "type" :: Parser String
        i <- v .: "ident" :: Parser String
        let
            sType =
                case typeName of
                    "area-db" -> AreaDB
                    "db" -> DB
                    "ws" -> WS
                    "http" -> HTTP
                    "admin" -> Admin
                    "area" -> Area
                    "log-aggregator" -> LogAggregator
                    _ -> Unknown
        return $ ServiceSettings sType i v
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


