{-# LANGUAGE DeriveGeneric #-}

module Area.Signal where

import GHC.Generics (Generic)
import Data.Sequence (Seq)

import Data.Aeson (ToJSON)

import Types (UserId)
import Area.Types (ObjId, Pos)


data AReason
    = LogIn
    | Entry
    | Recovery
    deriving (Generic, Eq, Ord)

instance ToJSON AReason


data DReason
    = LogOut
    | Exit
    | Destruction
    deriving (Generic, Eq, Ord)

instance ToJSON DReason


data Signal
    = Appearance {userId :: UserId, aReason :: AReason}
    | Disappearance {objId :: ObjId, dReason :: DReason}
    | Shot {shooterPos :: Pos, targetPos :: Pos}
    | MoveAsteroid {objId :: ObjId, targetPos :: Pos}
    deriving (Generic, Eq, Ord)

instance ToJSON Signal

type Signals = Seq Signal
