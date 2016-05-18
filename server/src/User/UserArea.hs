{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module User.UserArea where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Types


data UserArea = UserArea {
    userId :: UserId,
    name :: UserName,
    speed :: Int, --units per second
    maxDurability :: Int,
    durability :: Int,
    size :: !Size,
    asset :: !String,
    kills :: !Int,
    deaths :: !Int
    }
    deriving (Generic, Typeable)

instance Binary UserArea

