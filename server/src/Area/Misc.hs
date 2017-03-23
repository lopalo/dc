module Area.Misc where

import Control.Distributed.Process

import Base.GlobalCache (Value(AreaOwner), set, durationFactor)
import qualified Area.Settings as AS
import Area.State


updateOwnerName :: State -> Process ()
updateOwnerName state = set key value duration
    where
        key = show $ areaId state
        value = AreaOwner $ ownerName state
        s = settings state
        duration = AS.tickMilliseconds s * AS.syncEveryTick s * durationFactor
