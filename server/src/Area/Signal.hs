{-# LANGUAGE DeriveGeneric #-}

module Area.Signal where

import GHC.Generics (Generic)

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
    deriving (Generic, Eq, Ord)

instance ToJSON Signal

type Signals = [Signal]
