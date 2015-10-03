{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module App.User.External(UserArea(..), SyncState(..), SwitchArea(..),
                         monitorUser, syncState, switchArea) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (void)

import Control.Distributed.Process

import App.Types

data UserArea = UserArea {userId :: UserId,
                          name :: UserName,
                          speed :: Int, --units per second
                          durability :: Int}
                deriving (Generic, Typeable)
instance Binary UserArea


newtype SyncState = SyncState UserArea deriving (Generic, Typeable)
instance Binary SyncState

newtype SwitchArea = SwitchArea AreaId deriving (Generic, Typeable)
instance Binary SwitchArea

monitorUser :: UserPid -> Process ()
monitorUser (UserPid pid) = void $ monitor pid

syncState :: UserPid -> UserArea -> Process ()
syncState (UserPid pid) user = send pid $ SyncState user

switchArea :: UserPid -> AreaId -> Process ()
switchArea (UserPid pid) aid = send pid $ SwitchArea aid


