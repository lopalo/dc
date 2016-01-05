{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.Map.Strict as M

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Aeson.Types (Parser)

import Types (AreaId)
import qualified Area.Settings
import qualified User.Settings
import qualified HTTP.Settings
import qualified Admin.Settings


data Settings = Settings {
    user :: User.Settings.Settings,
    area :: Area.Settings.Settings,
    http :: HTTP.Settings.Settings,
    admin :: Admin.Settings.Settings,
    nodes :: M.Map String NodeSettings
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "user" <*>
        v .: "area" <*>
        v .: "http" <*>
        v .: "admin" <*>
        v .: "nodes"
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
            _ -> mzero
    parseJSON _ = mzero





