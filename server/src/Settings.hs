{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data Settings = Settings {areas :: [String],
                          startArea :: String,
                          nodeAddress :: (String, String),
                          wsAddress :: (String, Int),
                          user :: UserSettings,
                          area :: AreaSettings}

instance FromJSON Settings where
    parseJSON (Object v) = do
        s <- Settings <$>
             v .: "areas" <*>
             v .: "start-area" <*>
             v .: "node-address" <*>
             v .: "ws-address"
        uSettings <- parseJSON =<< (v .: "user")
        aSettings <- parseJSON =<< (v .: "area")
        return $ s uSettings aSettings
    parseJSON _ = mzero


data UserSettings =
    USettings {speed :: Int,
               initDurability :: Int}

instance FromJSON UserSettings where
    parseJSON (Object v) = USettings <$>
                           v .: "speed" <*>
                           v .: "init-durability"
    parseJSON _ = mzero


data AreaSettings =
    ASettings {enterPos :: (Int, Int),
               tickMilliseconds :: Int,
               broadcastEveryTick :: Int,
               logEveryTick :: Int}

instance FromJSON AreaSettings where
    parseJSON (Object v) = ASettings <$>
                           v .: "enter-pos" <*>
                           v .: "tick-milliseconds" <*>
                           v .: "broadcast-every-tick" <*>
                           v .: "log-every-tick"
    parseJSON _ = mzero


