{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Area.Objects.Asteroid where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Control.Applicative ((<$>), (<*>))
import Data.Text.Lazy.Encoding (encodeUtf8)

import Database.SQLite.Simple (FromRow(fromRow), ToRow(toRow), field)
import Database.SQLite.Simple.Internal (RowParser)
import Data.Aeson (Value, encode, decode, object, (.=))

import Area.Types (Object(..), Destroyable(..),
                   Pos(Pos), Angle, ObjId(AsteroidId))
import Area.Action (Active(..),
                    Action(EternalRotation, MoveCircularTrajectory),
                    eternalRotation, moveCircularTrajectory, publicAction)


data Asteroid = Asteroid {ident :: !ObjId,
                          name :: !String,
                          pos :: !Pos,
                          angle :: !Angle,
                          maxDurability :: !Int,
                          durability :: !Int,
                          actions :: ![Action]}
                deriving (Generic, Typeable)
instance Binary Asteroid


instance FromRow Asteroid where
    fromRow = do
        field :: RowParser String -- area field
        ident <- AsteroidId <$> field
        name <- field
        pos <- Pos <$> field <*> field
        angle <- field
        maxDurability <- field
        durability <- field
        Just actions <- decode . encodeUtf8 <$> field
        return Asteroid{ident=ident,
                        name=name,
                        pos=pos,
                        angle=angle,
                        maxDurability=maxDurability,
                        durability=durability,
                        actions=actions}

instance ToRow Asteroid where
    toRow g = toRow (id, name g, x, y, angle g, actions_)
        where AsteroidId id = ident g
              Pos x y = pos g
              actions_ = encode $ actions g


instance Object Asteroid where
    objId = ident


    getPos = pos
    setPos p ast = ast{pos=p}

    getAngle = angle
    setAngle a ast = ast{angle=a}

    initClientInfo ast =
        object ["id" .= ident ast,
                "tag" .= ("Asteroid" :: String),
                "name" .= name ast,
                "max-durability" .= maxDurability ast,
                "durability" .= durability ast,
                "angle" .= angle ast,
                "pos" .= pos ast]

    tickClientInfo ast =
        object ["id" .= ident ast,
                "pos" .= pos ast,
                "angle" .= angle ast,
                "durability" .= durability ast,
                "actions" .= filter publicAction (actions ast)]


instance Active Asteroid where


    apply ast action@EternalRotation{} = eternalRotation ast action
    apply ast action@MoveCircularTrajectory{} =
            moveCircularTrajectory ast action


    getActions = actions

    setActions as ast = ast{actions=as}

