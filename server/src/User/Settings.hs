{-# LANGUAGE OverloadedStrings #-}

module User.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import Types (Ts, AreaId, Size)


data Settings = Settings {
    startArea :: AreaId,
    size :: Size,
    speed :: Int,
    initDurability :: Int,
    logoutSeconds :: Int,
    periodMilliseconds :: Ts
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "start-area" <*>
        v .: "size" <*>
        v .: "speed" <*>
        v .: "init-durability" <*>
        v .: "logout-seconds" <*>
        v .: "period-milliseconds"
    parseJSON _ = mzero

