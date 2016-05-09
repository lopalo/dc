{-# LANGUAGE OverloadedStrings #-}

module Admin.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data Settings = Settings {uiDir :: String, verbose :: Int}

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "ui-dir" <*>
        v .: "verbose"
    parseJSON _ = mzero

