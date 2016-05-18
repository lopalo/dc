{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Area.Objects.Gate where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Aeson (Value(Object), object, (.=), (.:), (.:?), (.!=))

import Types (Size)
import DB.Types (Persistent(toDB, fromDB))
import Area.Types (Positioned(..), Object(..), Pos, Angle, ObjId)
import Area.Action (
    Active(..), Action(EternalRotation),
    eternalRotation, publicAction
    )


data Gate = Gate {
    ident :: !ObjId,
    name :: !String,
    pos :: !Pos,
    angle :: !Angle,
    actions :: ![Action],
    size :: !Size
    }
    deriving (Generic, Typeable)

instance Binary Gate


instance Persistent Gate where

    toDB g =
        object [
            "id" .= ident g,
            "name" .= name g,
            "pos" .= pos g,
            "angle" .= angle g,
            "actions" .= actions g,
            "size" .= size g
            ]

    fromDB (Object v) =
        Gate <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "pos" <*>
        v .:? "angle" .!= 0 <*>
        v .:? "actions" .!= [] <*>
        v .: "size"
    fromDB _ = mzero


instance Positioned Gate where

    getPos = pos

    setPos p gate = gate{pos=p}


instance Object Gate where

    objId = ident

    getAngle = angle

    setAngle a gate = gate{angle=a}

    initClientInfo gate =
        object [
            "id" .= ident gate,
            "tag" .= ("Gate" :: String),
            "name" .= name gate,
            "angle" .= angle gate,
            "pos" .= pos gate,
            "size" .= size gate,
            "actions" .= filter publicAction (actions gate)
            ]

    tickClientInfo gate =
        object [
            "id" .= ident gate,
            "angle" .= angle gate
            ]


instance Active Gate where

    apply gate action@EternalRotation{} = eternalRotation gate action

    getActions = actions

    setActions as gate = gate{actions=as}


