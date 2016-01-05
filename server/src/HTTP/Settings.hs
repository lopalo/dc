{-# LANGUAGE OverloadedStrings #-}

module HTTP.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data Settings = Settings {
    clientDir :: String,
    clientSettings :: String,
    wsAddresses :: [(String, Int)]
    }


instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "client-dir" <*>
        v .: "client-settings" <*>
        v .: "ws-addresses"
    parseJSON _ = mzero

