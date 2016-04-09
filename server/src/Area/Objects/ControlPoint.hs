{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Area.Objects.ControlPoint where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))

import Data.Aeson (Value(Object), object, (.=), (.:), (.:?), (.!=))

import Types (UserId, UserName, Size, width)
import DB.Types (Persistent(toDB, fromDB))
import Area.Types (
    Positioned(..), Object(..), Destroyable(..),
    Pos, Angle, ObjId
    )
import Area.Action (
    Active(..),
    Action(EternalRotation),
    eternalRotation, publicAction
    )
import Area.Collision (Collidable(collider), Collider(Circular))


data ControlPoint = ControlPoint {
    ident :: !ObjId,
    name :: !String,
    pos :: !Pos,
    angle :: !Angle,
    maxDurability :: !Int,
    durability :: !Int,
    actions :: ![Action],
    size :: !Size,
    owner :: Maybe (UserId, UserName)
    }
    deriving (Generic, Typeable)

instance Binary ControlPoint


instance Persistent ControlPoint where

    toDB cp =
        object [
            "id" .= ident cp,
            "name" .= name cp,
            "pos" .= pos cp,
            "angle" .= angle cp,
            "max-durability" .= maxDurability cp,
            "durability" .= durability cp,
            "actions" .= actions cp,
            "size" .= size cp,
            "owner" .= owner cp
            ]

    fromDB (Object v) =
        ControlPoint <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "pos" <*>
        v .:? "angle" .!= 0 <*>
        v .: "max-durability" <*>
        v .: "durability" <*>
        v .:? "actions" .!= [] <*>
        v .: "size" <*>
        v .:? "owner" .!= Nothing
    fromDB _ = mzero


instance Positioned ControlPoint where

    getPos = pos

    setPos p cp = cp{pos=p}


instance Object ControlPoint where

    objId = ident

    getAngle = angle

    setAngle a cp = cp{angle=a}

    initClientInfo cp =
        object [
            "id" .= ident cp,
            "tag" .= ("CP" :: String),
            "name" .= name cp,
            "max-durability" .= maxDurability cp,
            "durability" .= durability cp,
            "angle" .= angle cp,
            "pos" .= pos cp,
            "actions" .= filter publicAction (actions cp),
            "size" .= size cp,
            "owner" .= owner cp
            ]

    tickClientInfo cp =
        object [
            "id" .= ident cp,
            "pos" .= pos cp,
            "angle" .= angle cp,
            "durability" .= durability cp,
            "owner" .= (snd <$> owner cp)
            ]


instance Active ControlPoint where

    apply cp action@EternalRotation{} = eternalRotation cp action

    getActions = actions

    setActions as cp = cp{actions=as}


instance Destroyable ControlPoint where

    getMaxDurability = maxDurability

    setMaxDurability d cp = cp{maxDurability=d}

    getDurability = durability

    setDurability d cp = cp{durability=d}


instance Collidable ControlPoint where

    collider cp = Circular (objId cp) (getPos cp) (quot d 4)
        where d = width $ size cp
