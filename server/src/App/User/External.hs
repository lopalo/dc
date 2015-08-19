{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module App.User.External(UserArea(..), SyncState(..),
                         monitorUser, syncState) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (void)

import Control.Distributed.Process

import App.Types

data UserArea = UserArea {userId :: UserId,
                          name :: UserName,
                          area :: AreaId,
                          speed :: Int, --units per second
                          durability :: Int}
                deriving (Generic, Typeable)
instance Binary UserArea


newtype SyncState = SyncState UserArea deriving (Generic, Typeable)
instance Binary SyncState


monitorUser :: UserPid -> Process ()
monitorUser (UserPid pid) = void $ monitor pid


syncState :: UserPid -> UserArea -> Process ()
syncState (UserPid pid) user = send pid $ SyncState user

