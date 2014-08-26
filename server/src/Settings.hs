{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data Settings = Settings {areas :: [String],
                          startArea :: String,
                          areaUserSpeed :: Int,
                          initUserDurability :: Int,
                          startAreaPos :: (Int, Int),
                          areaTickMilliseconds :: Int,
                          areaBroadcastEveryTick :: Int}

instance FromJSON Settings where
    parseJSON (Object v) = Settings <$>
                           v .: "areas" <*>
                           v .: "start-area" <*>
                           v .: "area-user-speed" <*>
                           v .: "init-user-durability" <*>
                           v .: "start-area-pos" <*>
                           v .: "area-tick-milliseconds" <*>
                           v .: "area-broadcast-every-tick"
    parseJSON _ = mzero
