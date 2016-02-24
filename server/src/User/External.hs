{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module User.External(
    UserArea(..), SyncState(..), SwitchArea(..),
    monitorUser, syncState, switchArea
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Control.Distributed.Process

import Types


data UserArea = UserArea {
    userId :: UserId,
    name :: UserName,
    speed :: Int, --units per second
    maxDurability :: Int,
    durability :: Int,
    size :: !Size,
    kills :: !Int,
    deaths :: !Int
    }
    deriving (Generic, Typeable)

instance Binary UserArea


newtype SyncState = SyncState UserArea deriving (Generic, Typeable)

instance Binary SyncState


newtype SwitchArea = SwitchArea AreaId deriving (Generic, Typeable)

instance Binary SwitchArea


monitorUser :: UserPid -> Process UserMonitorRef
monitorUser (UserPid pid) = monitor pid


syncState :: UserPid -> UserArea -> Process ()
syncState (UserPid pid) user = send pid $ SyncState user


switchArea :: UserPid -> AreaId -> Process ()
switchArea (UserPid pid) aid = send pid $ SwitchArea aid


