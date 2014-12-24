{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Types where


import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

import Types (UserId, UserPid(..), AreaId)
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

--------


data Pos = Pos Int Int deriving (Generic, Typeable)
instance Binary Pos

instance ToJSON Pos where
    toJSON (Pos x y) = toJSON (x, y)

instance FromJSON Pos where
    parseJSON val = do
        (x, y) <- parseJSON val
        return (Pos x y)






