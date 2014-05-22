{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types (UserId(UserId), UserPid(UserPid),
              AreaId, AreaPid(AreaPid)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Control.Distributed.Process

import Utils (delPrefix)



delIdPrefix :: String -> String -> String
delIdPrefix = delPrefix ":"


newtype UserId = UserId String deriving (Eq, Ord, Generic, Typeable)

instance Binary UserId

instance Show UserId where
    show (UserId str) = "user_id:" ++ str

instance Read UserId where
    readsPrec _ str = [(UserId ("user_id" `delIdPrefix` str), "")]


newtype UserPid = UserPid ProcessId  deriving (Generic, Typeable)

instance Binary UserPid


type AreaId = String

newtype AreaPid = AreaPid ProcessId  deriving (Generic, Typeable)

instance Binary AreaPid


