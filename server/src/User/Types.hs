{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module User.Types where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Database.SQLite.Simple (FromRow(fromRow), ToRow(toRow), field)

import qualified WS.Connection as C
import Types (UserId(..), UserName, AreaId(AreaId), Size(Size))


----messages----

data Period = Period deriving (Generic, Typeable)

instance Binary Period


data Reconnection = Reconnection C.Connection deriving (Generic, Typeable)

instance Binary Reconnection


newtype SwitchArea = SwitchArea AreaId deriving (Generic, Typeable)

instance Binary SwitchArea


data UserMessage = UserMessage {
    sender :: !UserId,
    senderName :: !UserName,
    text :: !String
    }
    deriving (Generic, Typeable)

instance Binary UserMessage

instance ToJSON UserMessage where

    toJSON msg =
        toJSON $ object [
            "id" .= sender msg,
            "name" .= senderName msg,
            "text" .= text msg
            ]


data AreaOwnerName =
    AreaOwnerName AreaId (Maybe UserName)
    deriving (Generic, Typeable)

instance Binary AreaOwnerName


data ClientCommand
    = SendUserMessage [UserId] String
    deriving (Generic, Typeable)

instance Binary ClientCommand

--------


data User = User {
    userId :: !UserId,
    name :: !UserName,
    area :: !AreaId,
    speed :: !Int, --units per second
    maxDurability :: !Int,
    durability :: !Int,
    size :: !Size,
    kills :: !Int,
    deaths :: !Int
    }
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

    toRow u =
        toRow (
            uid,
            name u,
            aid,
            speed u,
            maxDurability u,
            durability u,
            w,
            h,
            kills u,
            deaths u
            )
        where
            UserId uid = userId u
            AreaId aid = area u
            Size w h = size u

