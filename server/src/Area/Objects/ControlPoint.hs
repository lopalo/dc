{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Area.Objects.ControlPoint where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Control.Applicative ((<$>), (<*>))
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Database.SQLite.Simple (FromRow(fromRow), ToRow(toRow), field)
import Database.SQLite.Simple.ToField (toField)
import Database.SQLite.Simple.Internal (RowParser)
import Data.Aeson (encode, decode, object, (.=))

import Types (UserId(UserId), Size(Size), width)
import Area.Types (
    Object(..), Destroyable(..),
    Pos(Pos), Angle, ObjId(CPId)
    )
import Area.Action (
    Active(..),
    Action(EternalRotation, MoveCircularTrajectory),
    eternalRotation, moveCircularTrajectory, publicAction
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
    owner :: Maybe UserId
    }
    deriving (Generic, Typeable)

instance Binary ControlPoint


instance FromRow ControlPoint where

    fromRow = do
        field :: RowParser String -- area field
        ident_ <- CPId <$> field
        name_ <- field
        pos_ <- Pos <$> field <*> field
        angle_ <- field
        maxDurability_ <- field
        durability_ <- field
        Just actions_ <- decode . encodeUtf8 <$> field
        size_ <- Size <$> field <*> field
        owner_ <- field
        return $
            ControlPoint{
                ident=ident_,
                name=name_,
                pos=pos_,
                angle=angle_,
                maxDurability=maxDurability_,
                durability=durability_,
                actions=actions_,
                size=size_,
                owner=UserId <$> owner_
                }

instance ToRow ControlPoint where

    toRow cp = [
            toField ident_,
            toField $ name cp,
            toField x,
            toField y,
            toField $ angle cp,
            toField $ maxDurability cp,
            toField $ durability cp,
            toField $ actions_,
            toField w,
            toField h,
            toField $ (\(UserId uid) -> uid) <$> owner cp
            ]
        where
            CPId ident_ = ident cp
            Pos x y = pos cp
            Size w h = size cp
            actions_ = decodeUtf8 $ encode $ actions cp


instance Object ControlPoint where

    objId = ident

    getPos = pos
    setPos p cp = cp{pos=p}

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
            "owner" .= owner cp
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
