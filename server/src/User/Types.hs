{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module User.Types where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)


import Control.Applicative ((<$>), (<*>))

import Database.SQLite.Simple (FromRow(fromRow), ToRow(toRow), field)

import Types (UserId(..), UserName, AreaId(AreaId), Size(Size))


data User = User {userId :: !UserId,
                  name :: !UserName,
                  area :: !AreaId,
                  speed :: !Int, --units per second
                  maxDurability :: !Int,
                  durability :: !Int,
                  size :: !Size,
                  kills :: !Int,
                  deaths :: !Int}
            deriving (Generic, Typeable)

instance Binary User


instance FromRow User where
    fromRow =
        let
            idField = UserId <$> field
            areaField = AreaId <$> field
            sizeField = Size <$> field <*> field
        in
            User <$>
            idField <*>
            field <*>
            areaField <*>
            field <*>
            field <*>
            field <*>
            sizeField <*>
            field <*>
            field


instance ToRow User where
    toRow u = toRow (uid,
                     name u,
                     aid,
                     speed u,
                     maxDurability u,
                     durability u,
                     w,
                     h,
                     kills u,
                     deaths u)
        where UserId uid = userId u
              AreaId aid = area u
              Size w h = size u

