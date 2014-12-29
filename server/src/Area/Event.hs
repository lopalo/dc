{-# LANGUAGE DeriveGeneric #-}

module Area.Event where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Types (UserId)
import Area.Types (ObjId)


data AReason = LogIn | Entry | Recovery
               deriving (Generic)

instance ToJSON AReason


data DReason = LogOut | Exit | Burst
               deriving (Generic)

instance ToJSON DReason


data Event = Appearance {userId :: UserId,
                         aReason :: AReason}
           | Disappearance {objId :: ObjId,
                            dReason :: DReason}
           | Shot {shooter :: UserId,
                   target :: UserId}
           deriving (Generic)

instance ToJSON Event

type Events = [Event]
