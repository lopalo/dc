{-# LANGUAGE OverloadedStrings #-}

module User.Settings where

import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import Types (Ts, AreaId(AreaId), Size)


data Settings = Settings {
    startArea :: AreaId,
    size :: Size,
    speed :: Int,
    initDurability :: Int,
    passwordHashSalt :: Int,
    logoutSeconds :: Int,
    periodMilliseconds :: Ts,
    minDBReplicas :: Int
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        (AreaId <$> v .: "start-area") <*>
        v .: "size" <*>
        v .: "speed" <*>
        v .: "init-durability" <*>
        v .: "password-hash-salt" <*>
        v .: "logout-seconds" <*>
        v .: "period-milliseconds" <*>
        v .: "min-db-replicas"
    parseJSON _ = mzero

