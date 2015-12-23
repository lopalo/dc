{-# LANGUAGE OverloadedStrings #-}

module User.Settings where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import Utils (Ts)
import Types (Size)


data Settings = Settings {
    size :: Size,
    speed :: Int,
    initDurability :: Int,
    logoutSeconds :: Int,
    periodMilliseconds :: Ts
    }

instance FromJSON Settings where

    parseJSON (Object v) =
        Settings <$>
        v .: "size" <*>
        v .: "speed" <*>
        v .: "init-durability" <*>
        v .: "logout-seconds" <*>
        v .: "period-milliseconds"
    parseJSON _ = mzero

