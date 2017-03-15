{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module User.Types where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (mzero)

import Data.Aeson (
    ToJSON(toJSON), Value(Object),
    object, (.=), (.:), (.:?), (.!=)
    )

import qualified WS.Connection as C
import DB.Types (Persistent(toDB, fromDB))
import Types (UserId(..), UserName, AreaId, Size)


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
    asset :: !String,
    kills :: !Int,
    deaths :: !Int
    }
    deriving (Generic, Typeable)

instance Binary User


instance Persistent User where

    toDB u =
        object [
            "id" .= userId u,
            "name" .= name u,
            "area" .= area u,
            "speed" .= speed u,
            "max-durability" .= maxDurability u,
            "durability" .= durability u,
            "size" .= size u,
            "asset" .= asset u,
            "kills" .= kills u,
            "deaths" .= deaths u
            ]

    fromDB (Object v) =
        User <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "area" <*>
        v .: "speed" <*>
        v .: "max-durability" <*>
        v .: "durability" <*>
        v .: "size" <*>
        v .: "asset" <*>
        v .:? "kills" .!= 0 <*>
        v .:? "deaths" .!= 0
    fromDB _ = mzero


