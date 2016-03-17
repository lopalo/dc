module Area.Misc where

import qualified Data.Map.Strict as M
import System.Random (randomR)

import Data.Lens.Partial.Common ((^%=))
import Control.Distributed.Process

import Types (UserId, width, userPrefix)
import Utils (mkRandomGen, choice)
import qualified User.External as UE
import qualified Area.Settings as AS
import Area.State
import Area.Objects.Gate (pos, size)
import Area.Vector (toPos, fromPos, fromPolar, add)
import qualified Area.Objects.User as U


spawnUser :: UserId -> State -> State
spawnUser uid state =
    let rGen = mkRandomGen uid $ tickNumber state
        gs = M.elems $ gates state
        (gate, rGen') = choice gs rGen
        minRadius = fromIntegral $ width $ size gate
        maxRadius = (AS.gateFieldRadius . settings) state
        (radius, rGen'') = randomR (minRadius, maxRadius) rGen'
        (spawnAngle, rGen''') = randomR (0, 360) rGen''
        (angle, _) = randomR (0, 360) rGen'''
        spawnPos = toPos $ fromPos (pos gate) `add` fromPolar radius spawnAngle
        modUser u = u{U.pos=spawnPos, U.angle=angle}
    in
        if null gs
            then state
            else (userPL uid ^%= modUser) state


broadcastOwnerName :: State -> Process ()
broadcastOwnerName state =
    UE.broadcastAreaOwnerName (areaId state) (ownerName state)
