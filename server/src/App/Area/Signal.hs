{-# LANGUAGE DeriveGeneric #-}

module App.Area.Signal where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import App.Types (UserId)
import App.Area.Types (ObjId)


data AReason = LogIn | Entry | Recovery
               deriving (Generic)

instance ToJSON AReason


data DReason = LogOut | Exit | Burst
               deriving (Generic)

instance ToJSON DReason


data Signal = Appearance {userId :: UserId,
                          aReason :: AReason}
            | Disappearance {objId :: ObjId,
                             dReason :: DReason}
            | Shot {shooter :: UserId,
                    target :: UserId}
            deriving (Generic)

instance ToJSON Signal

type Signals = [Signal]
