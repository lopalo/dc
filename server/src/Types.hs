{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Control.Distributed.Process

import Utils (delPrefix)


delIdPrefix :: String -> String -> String
delIdPrefix = delPrefix ":"

type RequestNumber = Int

type UserName = String

newtype UserId = UserId String deriving (Eq, Ord, Generic, Typeable)
instance Binary UserId

instance Show UserId where
    show (UserId str) = "user:" ++ str

instance Read UserId where
    readsPrec _ str = [(UserId ("user" `delIdPrefix` str), "")]

instance ToJSON UserId where
    toJSON = toJSON . show

instance FromJSON UserId where
    parseJSON val = read <$> parseJSON val


newtype UserPid = UserPid ProcessId  deriving (Eq, Ord, Generic, Typeable)
instance Binary UserPid


newtype AreaId = AreaId String deriving (Eq, Ord, Generic, Typeable)
instance Binary AreaId

instance Show AreaId where
    show (AreaId str) = "area:" ++ str

instance Read AreaId where
    readsPrec _ str = [(AreaId ("area" `delIdPrefix` str), "")]

instance ToJSON AreaId where
    toJSON = toJSON . show

instance FromJSON AreaId where
    parseJSON val = read <$> parseJSON val


newtype AreaPid = AreaPid ProcessId  deriving (Generic, Typeable)
instance Binary AreaPid


data Size = Size !Int !Int deriving (Generic, Typeable, Eq, Show)
instance Binary Size

instance ToJSON Size where
    toJSON (Size w h) = toJSON (w, h)

instance FromJSON Size where
    parseJSON val = do
        (w, h) <- parseJSON val
        return (Size w h)
