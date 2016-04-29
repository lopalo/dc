{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import Data.String.Utils (split, join)
import qualified Data.Map.Strict as M

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Control.Distributed.Process


delPrefix :: String -> String -> String -> String
delPrefix delimiter prefix str =
    case delimiter `split` str of
        h:rest | h == prefix -> delimiter `join` rest


delIdPrefix :: String -> String -> String
delIdPrefix = delPrefix ":"


type Ts = Int -- absolute time in milliseconds


type ServiceId = String


type NodeName = String


type NodeNames = M.Map NodeId NodeName


type RequestNumber = Int


type UserName = String


type UserMonitorRef = MonitorRef


type AreaOwners = M.Map AreaId (Maybe UserName)


newtype UserId = UserId String deriving (Eq, Ord, Generic, Typeable)

instance Binary UserId

instance Show UserId where

    show (UserId str) = prefix User ++ str

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

    show (AreaId str) = prefix Area ++ str

instance Read AreaId where

    readsPrec _ str = [(AreaId ("area" `delIdPrefix` str), "")]

instance ToJSON AreaId where

    toJSON = toJSON . show

instance FromJSON AreaId where

    parseJSON val = read <$> parseJSON val


newtype AreaPid = AreaPid ProcessId  deriving (Generic, Typeable)
instance Binary AreaPid


data LogLevel = Error | Info | Debug
    deriving (Generic, Typeable, Show, Eq, Ord, Enum, Bounded)

instance Binary LogLevel

instance FromJSON LogLevel


data Size =
    Size {width :: !Int, height :: !Int}
    deriving (Generic, Typeable, Eq, Show)

instance Binary Size

instance ToJSON Size where

    toJSON (Size w h) = toJSON (w, h)

instance FromJSON Size where

    parseJSON val = do
        (w, h) <- parseJSON val
        return (Size w h)


data AreaStatus = AreaStatus {
        statusAreaId :: AreaId,
        userAmount :: Int,
        objectAmount :: Int,
        tickDurationMs :: Int
    }
    deriving (Generic, Typeable)

instance Binary AreaStatus


data ServiceType
    = AreaDB
    | UserDB
    | WS
    | HTTP
    | Admin
    | Area
    | User
    | NodeAgent
    | LogAggregator
    | Unknown
    deriving (Eq, Ord)


servicePrefixes :: M.Map ServiceType String
servicePrefixes = M.fromList [
    (AreaDB, "area-db:"),
    (UserDB, "user-db:"),
    (WS, "ws:"),
    (HTTP, "http:"),
    (Admin, "admin:"),
    (Area, "area:"),
    (User, "user:"),
    (NodeAgent, "node:"),
    (LogAggregator, "log-aggregator:")
    ]


prefix :: ServiceType -> String
prefix = (servicePrefixes M.!)


