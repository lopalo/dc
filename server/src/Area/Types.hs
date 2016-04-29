{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Types where


import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.String.Utils (startswith)
import Data.Aeson (Value, ToJSON, toJSON, FromJSON, parseJSON)
import Data.Lens.Strict (Lens, lens)

import Types (
    UserId(..), UserPid(..), ServiceType(User),
    AreaId, delIdPrefix, prefix
    )
import qualified User.External as UE


type Angle = Float --degrees


----messages----

data Reconnection = Reconnection UserId deriving (Generic, Typeable)

instance Binary Reconnection


data Enter = Enter !UE.UserArea UserPid Bool deriving (Generic, Typeable)

instance Binary Enter


data GetOwner = GetOwner deriving (Generic, Typeable)

instance Binary GetOwner


data GetAreaStatus = GetAreaStatus deriving (Generic, Typeable)

instance Binary GetAreaStatus


data ClientCommand
    = Echo !String
    | GetObjectsInfo ![ObjId]
    | EnterArea !AreaId
    | MoveAlongRoute ![Pos]
    | Recover !Float
    | Shoot !Pos
    | Capture !ObjId
    | PullAsteroid !ObjId
    | CancelPull
    deriving (Generic, Typeable)

instance Binary ClientCommand

--------


data Pos = Pos !Int !Int deriving (Generic, Typeable, Eq, Ord, Show)

instance Binary Pos

instance ToJSON Pos where

    toJSON (Pos x y) = toJSON (x, y)

instance FromJSON Pos where

    parseJSON val = do
        (x, y) <- parseJSON val
        return (Pos x y)


data ObjId = UId UserId
           | GateId String
           | AsteroidId String
           | CPId String
           deriving (Eq, Ord, Generic, Typeable)

instance Binary ObjId

instance Show ObjId where

    show (UId uid) = show uid
    show (GateId ident) = "gate:" ++ ident
    show (AsteroidId ident) = "asteroid:" ++ ident
    show (CPId ident) = "cp:" ++ ident

instance Read ObjId where

    readsPrec _ str
        | prefix User `startswith` str = [(UId (read str), "")]
        | "gate:" `startswith` str = [(GateId ("gate" `delIdPrefix` str), "")]
        | "asteroid:" `startswith` str =
            [(AsteroidId ("asteroid" `delIdPrefix` str), "")]
        | "cp:" `startswith` str = [(CPId ("cp" `delIdPrefix` str), "")]

instance ToJSON ObjId where

    toJSON = toJSON . show

instance FromJSON ObjId where

    parseJSON val = do
        str <- parseJSON val
        return (read str)


class Positioned p where

    getPos :: p -> Pos

    setPos :: Pos -> p -> p


class Positioned o => Object o where

    objId :: o -> ObjId

    getAngle :: o -> Angle

    setAngle :: Angle -> o -> o

    initClientInfo :: o -> Value

    tickClientInfo :: o -> Value

    posL :: Lens o Pos
    posL = lens getPos setPos

    angleL :: Lens o Angle
    angleL = lens getAngle setAngle


class Object o => Destroyable o where

    getMaxDurability :: o -> Int

    setMaxDurability :: Int -> o -> o

    getDurability :: o -> Int

    setDurability :: Int -> o -> o


    durabilityL :: Lens o Int
    durabilityL = lens getDurability setDurability

