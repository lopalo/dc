{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types (UserId(UserId), UserPid(UserPid),
              AreaId, AreaPid(AreaPid), UserName) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Control.Distributed.Process

import Utils (delPrefix)


delIdPrefix :: String -> String -> String
delIdPrefix = delPrefix ":"


type UserName = String

newtype UserId = UserId String deriving (Eq, Ord, Generic, Typeable)
instance Binary UserId

instance Show UserId where
    show (UserId str) = "user_id:" ++ str

instance Read UserId where
    readsPrec _ str = [(UserId ("user_id" `delIdPrefix` str), "")]

instance ToJSON UserId where
    toJSON = toJSON . show

instance FromJSON UserId where
    parseJSON val = do
        str <- parseJSON val
        return (read str)


newtype UserPid = UserPid ProcessId  deriving (Generic, Typeable)
instance Binary UserPid


type AreaId = String

newtype AreaPid = AreaPid ProcessId  deriving (Generic, Typeable)
instance Binary AreaPid

