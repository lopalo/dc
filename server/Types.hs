{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types (UserId(UserId), UserPid(UserPid),
              AreaId, AreaPid(AreaPid)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.String.Utils (split, join)

import Control.Distributed.Process


delPrefix :: String -> String -> String
delPrefix prefix str =
    case ":" `split` str of
        h:rest | h == prefix-> ":" `join` rest


newtype UserId = UserId String deriving (Eq, Ord, Generic, Typeable)

instance Binary UserId

instance Show UserId where
    show (UserId str) = "user_id:" ++ str

instance Read UserId where
    readsPrec _ str = [(UserId ("user_id" `delPrefix` str), "")]


newtype UserPid = UserPid ProcessId  deriving (Generic, Typeable)

instance Binary UserPid


type AreaId = String

newtype AreaPid = AreaPid ProcessId  deriving (Generic, Typeable)

instance Binary AreaPid


