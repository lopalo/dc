{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Signal where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Sequence (Seq)

import Data.Aeson (ToJSON, FromJSON)

import Types (UserId, AreaId)
import Area.Types (ObjId, Pos)


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
    = Appearance {userId :: UserId, aReason :: AReason}
    | Disappearance {objId :: ObjId, dReason :: DReason}
    | Shot {shooterPos :: Pos, targetPos :: Pos}
    | MoveAsteroid {objId :: ObjId, targetPos :: Pos}
    | JumpToArea {areaId :: AreaId, userId :: UserId}
    | EmptySignal
    deriving (Generic, Typeable, Eq, Ord)

instance ToJSON Signal

instance FromJSON Signal

instance Binary Signal


type Signals = Seq Signal
