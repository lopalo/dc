{-# LANGUAGE OverloadedStrings #-}

module App.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import App.Utils (Ts)
import App.Types (AreaId)


data Settings = Settings {areas :: [AreaId],
                          startArea :: AreaId,
                          nodeAddress :: (String, String),
                          httpPort :: Int,
                          clientDir :: String,
                          wsAddress :: (String, Int),
                          wsAddresses :: [(String, Int)],
                          db :: String,
                          user :: UserSettings,
                          area :: AreaSettings}

instance FromJSON Settings where
    parseJSON (Object v) = do
        s <- Settings <$>
             v .: "areas" <*>
             v .: "start-area" <*>
             v .: "node-address" <*>
             v .: "http-port" <*>
             v .: "client-dir" <*>
             v .: "ws-address" <*>
             v .: "ws-addresses" <*>
             v .: "db"
        uSettings <- parseJSON =<< (v .: "user")
        aSettings <- parseJSON =<< (v .: "area")
        return $ s uSettings aSettings
    parseJSON _ = mzero


data UserSettings =
    USettings {speed :: Int,
               initDurability :: Int,
               logoutSeconds :: Int,
               periodMilliseconds :: Ts}

instance FromJSON UserSettings where
    parseJSON (Object v) = USettings <$>
                           v .: "speed" <*>
                           v .: "init-durability" <*>
                           v .: "logout-seconds" <*>
                           v .: "period-milliseconds"
    parseJSON _ = mzero


data AreaSettings =
    ASettings {enterPos :: (Int, Int),
               shotDamage :: Int,
               routeFilterThreshold :: Float,
               tickMilliseconds :: Ts,
               broadcastEveryTick :: Int,
               logEveryTick :: Int,
               syncEveryTick :: Int}

instance FromJSON AreaSettings where
    parseJSON (Object v) = ASettings <$>
                           v .: "enter-pos" <*>
                           v .: "shot-damage" <*>
                           v .: "route-filter-threshold" <*>
                           v .: "tick-milliseconds" <*>
                           v .: "broadcast-every-tick" <*>
                           v .: "log-every-tick" <*>
                           v .: "sync-every-tick"
    parseJSON _ = mzero


