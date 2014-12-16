{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module User.External where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (void)

import Control.Distributed.Process

import Types

data UserArea = UserArea {userId :: UserId,
                          name :: UserName,
                          area :: AreaId,
                          speed :: Int, --units per second
                          durability :: Int}
                deriving (Generic, Typeable)
instance Binary UserArea

monitorUser :: UserPid -> Process ()
monitorUser (UserPid pid) = void $ monitor pid

