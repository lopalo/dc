{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types (UserId, UserPid(UserPid), AreaId, AreaPid(AreaPid)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Control.Distributed.Process

type UserId = String

newtype UserPid = UserPid ProcessId  deriving (Generic, Typeable)
instance Binary UserPid


type AreaId = String

newtype AreaPid = AreaPid ProcessId  deriving (Generic, Typeable)
instance Binary AreaPid


