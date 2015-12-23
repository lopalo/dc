{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Types where


import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.String.Utils (startswith)
import Data.Aeson (Value, ToJSON, toJSON, FromJSON, parseJSON)
import Data.Lens.Strict (Lens, lens)

import Types (UserId(..), UserPid(..), AreaId, delIdPrefix)
import qualified User.External as UE


----messages----
data Reconnection = Reconnection UserId deriving (Generic, Typeable)

instance Binary Reconnection


data Enter = Enter !UE.UserArea UserPid Bool deriving (Generic, Typeable)

instance Binary Enter


data ClientCommand
    = Echo !String
    | GetObjectsInfo ![ObjId]
    | EnterArea !AreaId
    | MoveAlongRoute ![Pos]
    | Recover !Float
    | Shoot !Pos
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


type Angle = Float --degrees


data ObjId = UId UserId
           | GateId String
           | AsteroidId String
           deriving (Eq, Ord, Generic, Typeable)

instance Binary ObjId

instance Show ObjId where

    show (UId uid) = show uid
    show (GateId ident) = "gate:" ++ ident
    show (AsteroidId ident) = "asteroid:" ++ ident

instance Read ObjId where

    readsPrec _ str
        | "user:" `startswith` str = [(UId (read str), "")]
        | "gate:" `startswith` str = [(GateId ("gate" `delIdPrefix` str), "")]
        | "asteroid:" `startswith` str =
            [(AsteroidId ("asteroid" `delIdPrefix` str), "")]

instance ToJSON ObjId where

    toJSON = toJSON . show

instance FromJSON ObjId where

    parseJSON val = do
        str <- parseJSON val
        return (read str)


class Object o where

    objId :: o -> ObjId

    getPos :: o -> Pos

    setPos :: Pos -> o -> o

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


