{-# LANGUAGE OverloadedStrings #-}

module DB.Settings where

import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data Settings = Settings {
    dbDir :: String
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "db-dir"
    parseJSON _ = mzero

