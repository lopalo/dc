{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Area.Objects.Asteroid where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (mzero)

import Data.Aeson (Value(Object), object, (.=), (.:), (.:?), (.!=))

import Types (Size, width)
import DB.Types (Persistent(toDB, fromDB))
import Area.Types (
    Positioned(..), Object(..), Destroyable(..),
    Pos, Angle, ObjId
    )
import Area.Action (
    Active(..),
    Action(EternalRotation, MoveCircularTrajectory, MoveDistance),
    eternalRotation, moveCircularTrajectory, moveDistance,
    publicAction
    )
import Area.Collision (Collidable(collider), Collider(Circular))


data Asteroid = Asteroid {
    ident :: !ObjId,
    name :: !String,
    pos :: !Pos,
    angle :: !Angle,
    maxDurability :: !Int,
    durability :: !Int,
    actions :: ![Action],
    size :: !Size,
    asset :: !String
    }
    deriving (Generic, Typeable)

instance Binary Asteroid


instance Persistent Asteroid where

    toDB ast =
        object [
            "id" .= ident ast,
            "name" .= name ast,
            "pos" .= pos ast,
            "angle" .= angle ast,
            "max-durability" .= maxDurability ast,
            "durability" .= durability ast,
            "actions" .= actions ast,
            "size" .= size ast,
            "asset" .= asset ast
            ]

    fromDB (Object v) =
        Asteroid <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "pos" <*>
        v .:? "angle" .!= 0 <*>
        v .: "max-durability" <*>
        v .: "durability" <*>
        v .:? "actions" .!= [] <*>
        v .: "size" <*>
        v .: "asset"
    fromDB _ = mzero


instance Positioned Asteroid where

    getPos = pos

    setPos p ast = ast{pos=p}


instance Object Asteroid where

    objId = ident

    getAngle = angle

    setAngle a ast = ast{angle=a}

    initClientInfo ast =
        object [
            "id" .= ident ast,
            "tag" .= ("Asteroid" :: String),
            "name" .= name ast,
            "max-durability" .= maxDurability ast,
            "durability" .= durability ast,
            "angle" .= angle ast,
            "pos" .= pos ast,
            "actions" .= filter publicAction (actions ast),
            "size" .= size ast,
            "asset" .= asset ast
            ]

    tickClientInfo ast =
        object [
            "id" .= ident ast,
            "pos" .= pos ast,
            "angle" .= angle ast,
            "durability" .= durability ast,
            "actions" .= filter publicAction (actions ast)
            ]


instance Active Asteroid where

    apply ast action@EternalRotation{} = eternalRotation ast action
    apply ast action@MoveCircularTrajectory{} =
        moveCircularTrajectory ast action
    apply ast action@MoveDistance{} = moveDistance ast action

    getActions = actions

    setActions as ast = ast{actions=as}


instance Destroyable Asteroid where

    getMaxDurability = maxDurability

    setMaxDurability d ast = ast{maxDurability=d}

    getDurability = durability

    setDurability d ast = ast{durability=d}


instance Collidable Asteroid where

    collider ast = Circular (objId ast) (getPos ast) (quot d 2)
        where d = width $ size ast
