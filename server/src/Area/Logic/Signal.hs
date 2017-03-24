{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Logic.Signal where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Sequence (Seq)

import Data.Aeson (ToJSON, FromJSON)

import Types (UserId, AreaId)
import Area.Types (Positioned(..), ObjId, Pos)
import Area.Logic.Vector


data AReason
    = LogIn
    | Entry
    | Recovery
    deriving (Generic, Typeable, Eq, Ord)

instance ToJSON AReason

instance FromJSON AReason

instance Binary (AReason)


data DReason
    = LogOut
    | Exit
    | Destruction
    deriving (Generic, Typeable, Eq, Ord)

instance ToJSON DReason

instance FromJSON DReason

instance Binary (DReason)


data Signal
    = Appearance {userId :: UserId, aReason :: AReason, pos :: Pos}
    | Disappearance {objId :: ObjId, dReason :: DReason, pos :: Pos}
    | Shot {shooterPos :: Pos, targetPos :: Pos}
    | MoveAsteroid {objId :: ObjId, targetPos :: Pos}
    | JumpToArea {areaId :: AreaId, userId :: UserId}
    | EmptySignal
    deriving (Generic, Typeable, Eq, Ord)

instance ToJSON Signal

instance FromJSON Signal

instance Binary Signal

instance Positioned Signal where

    getPos Appearance{pos=p} = p
    getPos Disappearance{pos=p} = p
    getPos Shot{shooterPos=p, targetPos=p'} =
        toPos $ fromPos p `add` fromPos p' `divide` 2

    setPos p s = s{pos=p}


type Signals = Seq Signal
