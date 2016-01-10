{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Area.Objects.Gate where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Control.Applicative ((<$>), (<*>))
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Database.SQLite.Simple (FromRow(fromRow), ToRow(toRow), field)
import Database.SQLite.Simple.Internal (RowParser)
import Data.Aeson (encode, decode, object, (.=))

import Types (Size(Size))
import Area.Types (Object(..), Pos(Pos), Angle, ObjId(GateId))
import Area.Action (Active(..), Action(EternalRotation), eternalRotation)


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


instance FromRow Gate where

    fromRow = do
        field :: RowParser String -- area field
        ident_ <- GateId <$> field
        name_ <- field
        pos_ <- Pos <$> field <*> field
        angle_ <- field
        Just actions_ <- decode . encodeUtf8 <$> field
        size_ <- Size <$> field <*> field
        return $ Gate{
            ident=ident_,
            name=name_,
            pos=pos_,
            angle=angle_,
            actions=actions_,
            size=size_
            }

instance ToRow Gate where

    toRow g = toRow (ident_, name g, x, y, angle g, actions_, w, h)
        where
            GateId ident_ = ident g
            Pos x y = pos g
            Size w h = size g
            actions_ = decodeUtf8 $ encode $ actions g


instance Object Gate where

    objId = ident

    getPos = pos

    setPos p gate = gate{pos=p}

    getAngle = angle

    setAngle a gate = gate{angle=a}

    initClientInfo gate =
        object [
            "id" .= ident gate,
            "tag" .= ("Gate" :: String),
            "name" .= name gate,
            "angle" .= angle gate,
            "pos" .= pos gate,
            "size" .= size gate
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


