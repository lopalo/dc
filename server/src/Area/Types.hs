{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Types (Ts, Pos(Pos)) where


import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

type Ts = Int -- time in milliseconds


data Pos = Pos Int Int deriving (Generic, Typeable)
instance Binary Pos

instance ToJSON Pos where
    toJSON (Pos x y) = toJSON (x, y)

instance FromJSON Pos where
    parseJSON val = do
        (x, y) <- parseJSON val
        return (Pos x y)






