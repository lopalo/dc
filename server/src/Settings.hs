{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import Types (AreaId)
import qualified Area.Settings
import qualified User.Settings
import qualified Admin.Settings


data Settings = Settings {
    areas :: [AreaId],
    startArea :: AreaId,
    nodeAddress :: (String, String),
    httpPort :: Int,
    clientDir :: String,
    clientSettings :: String,
    wsAddress :: (String, Int),
    wsAddresses :: [(String, Int)],
    db :: String,
    user :: User.Settings.Settings,
    area :: Area.Settings.Settings,
    admin :: Admin.Settings.Settings
    }

instance FromJSON Settings where

    parseJSON (Object v) = do
        settings <-
            Settings <$>
            v .: "areas" <*>
            v .: "start-area" <*>
            v .: "node-address" <*>
            v .: "http-port" <*>
            v .: "client-dir" <*>
            v .: "client-settings" <*>
            v .: "ws-address" <*>
            v .: "ws-addresses" <*>
            v .: "db"
        uSettings <- parseJSON =<< (v .: "user")
        aSettings <- parseJSON =<< (v .: "area")
        adminSettings <- parseJSON =<< (v .: "admin")
        return $ settings uSettings aSettings adminSettings
    parseJSON _ = mzero






