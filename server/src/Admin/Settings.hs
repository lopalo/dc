{-# LANGUAGE OverloadedStrings #-}

module Admin.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data Settings = Settings {port :: Int,
                          staticDir :: String,
                          templateDir :: String}

instance FromJSON Settings where
    parseJSON (Object v) = Settings <$>
             v .: "port" <*>
             v .: "static-dir" <*>
             v .: "template-dir"
    parseJSON _ = mzero


