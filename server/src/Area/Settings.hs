{-# LANGUAGE OverloadedStrings #-}

module Area.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import Utils (Ts)


data Settings = Settings {
    gateFieldRadius :: Float,
    shotDamage :: Int,
    shotDistance :: Float,
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
        v .: "shot-damage" <*>
        v .: "shot-distance" <*>
        v .: "route-filter-threshold" <*>
        v .: "tick-milliseconds" <*>
        v .: "broadcast-every-tick" <*>
        v .: "log-every-tick" <*>
        v .: "sync-every-tick"
    parseJSON _ = mzero

