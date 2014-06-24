{-# LANGUAGE DeriveGeneric #-}

module Area.Event (Event(..), Events) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Types (UserId)


data Event = DeleteUser {ident :: UserId}
           | Dummy
           deriving (Generic)

instance ToJSON Event

type Events = [Event]
