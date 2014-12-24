{-# LANGUAGE DeriveGeneric #-}

module Area.Event where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Types (UserId)


data AReason = LogIn | Entry | Recovery
               deriving (Generic)

instance ToJSON AReason


data DReason = LogOut | Exit | Burst
               deriving (Generic)

instance ToJSON DReason


data Event = Appearance {ident :: UserId,
                         aReason :: AReason}
           | Disappearance {ident :: UserId,
                            dReason :: DReason}
           deriving (Generic)

instance ToJSON Event

type Events = [Event]
