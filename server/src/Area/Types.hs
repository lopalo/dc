{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Types where


import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Control.Distributed.Process hiding (forward)

import Connection (Connection, setArea, sendResponse)
import Types (UserId, UserPid(..), AreaId, AreaPid(..), RequestNumber)
import qualified User.External as UE


type ForwardData = (ProcessId, Connection, RequestNumber)



data Echo = Echo !String deriving (Generic, Typeable)
instance Binary Echo

data Enter = Enter !UE.UserArea UserPid deriving (Generic, Typeable)
instance Binary Enter

data GetObjectsInfo = GetObjectsInfo ![String] deriving (Generic, Typeable)
instance Binary GetObjectsInfo

data EnterArea = EnterArea !AreaId deriving (Generic, Typeable)
instance Binary EnterArea

data MoveTo = MoveTo !Pos deriving (Generic, Typeable)
instance Binary MoveTo

data Ignite = Ignite !Float deriving (Generic, Typeable)
instance Binary Ignite


data Pos = Pos Int Int deriving (Generic, Typeable)
instance Binary Pos

instance ToJSON Pos where
    toJSON (Pos x y) = toJSON (x, y)

instance FromJSON Pos where
    parseJSON val = do
        (x, y) <- parseJSON val
        return (Pos x y)






