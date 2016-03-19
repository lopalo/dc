{-# LANGUAGE OverloadedStrings #-}

module Area.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import Types (Ts)


data Settings = Settings {
    gateFieldRadius :: Float,
    shot :: ShotSettings,
    asteroidPullSpeed :: Float,
    routeFilterThreshold :: Float,
    tickMilliseconds :: Ts,
    broadcastEveryTick :: Int,
    logEveryTick :: Int,
    syncEveryTick :: Int
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "gate-field-radius" <*>
        v .: "shot" <*>
        v .: "asteroid-pull-speed" <*>
        v .: "route-filter-threshold" <*>
        v .: "tick-milliseconds" <*>
        v .: "broadcast-every-tick" <*>
        v .: "log-every-tick" <*>
        v .: "sync-every-tick"
    parseJSON _ = mzero


data ShotSettings = ShotSettings {
    shotDamage :: Int,
    shotDistance :: Float,
    shotCooldownMilliseconds :: Ts
    }

instance FromJSON ShotSettings where

    parseJSON (Object v) =
        ShotSettings <$>
        v .: "damage" <*>
        v .: "distance" <*>
        v .: "cooldown-milliseconds"
    parseJSON _ = mzero


