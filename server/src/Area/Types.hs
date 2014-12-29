{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Types where


import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.String.Utils (startswith)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

import Types (UserId(..), UserPid(..), AreaId, delIdPrefix)
import qualified User.External as UE


----messages----

data Reconnection = Reconnection UserId deriving (Generic, Typeable)
instance Binary Reconnection

data Echo = Echo !String deriving (Generic, Typeable)
instance Binary Echo

data Enter = Enter !UE.UserArea UserPid Bool deriving (Generic, Typeable)
instance Binary Enter

data GetObjectsInfo = GetObjectsInfo ![String] deriving (Generic, Typeable)
instance Binary GetObjectsInfo

data EnterArea = EnterArea !AreaId deriving (Generic, Typeable)
instance Binary EnterArea

data MoveTo = MoveTo !Pos deriving (Generic, Typeable)
instance Binary MoveTo

data Ignite = Ignite !Float deriving (Generic, Typeable)
instance Binary Ignite

data Shoot = Shoot !UserId deriving (Generic, Typeable)
instance Binary Shoot


--------


data Pos = Pos Int Int deriving (Generic, Typeable)
instance Binary Pos

instance ToJSON Pos where
    toJSON (Pos x y) = toJSON (x, y)

instance FromJSON Pos where
    parseJSON val = do
        (x, y) <- parseJSON val
        return (Pos x y)


data ObjId = UId UserId | AsteroidId String deriving (Eq, Ord)

instance Show ObjId where
    show (UId uid) = show uid
    show (AsteroidId ident) = "asteroid-id:" ++ ident

instance Read ObjId where
    readsPrec _ str
        | "user-id:" `startswith` str = [(UId (read str), "")]
        | "asteroid-id:" `startswith` str =
            [(AsteroidId ("asteroid-id" `delIdPrefix` str), "")]

instance ToJSON ObjId where
    toJSON = toJSON . show

instance FromJSON ObjId where
    parseJSON val = do
        str <- parseJSON val
        return (read str)


class Object o where
    objId :: o -> ObjId


class Object o => Destroyable o where
    objDurability :: o -> Int
