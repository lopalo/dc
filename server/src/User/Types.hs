{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module User.Types where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)


import Control.Applicative ((<$>))
import Control.Monad (liftM4)

import Database.SQLite.Simple (FromRow(fromRow), ToRow(toRow), field)

import Types (UserId(..), UserName, AreaId)


data User = User {userId :: UserId,
                  name :: UserName,
                  area :: AreaId,
                  speed :: Int, --units per second
                  durability :: Int}
            deriving (Generic, Typeable)

instance Binary User


instance FromRow User where
    fromRow = do
        usr <- User <$> (UserId <$> field)
        liftM4 usr field field field field


instance ToRow User where
    toRow u = toRow (uid, name u, area u, speed u, durability u)
        where UserId uid = userId u

