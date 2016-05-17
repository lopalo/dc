module Area.Misc where

import qualified Data.Map.Strict as M
import System.Random (randomR)

import Data.Lens.Partial.Common ((^%=))
import Control.Distributed.Process

import Types (AreaId(AreaId), UserId, width)
import Utils (mkRandomGen, choice)
import Base.GlobalCache (Value(AreaOwner), set, durationFactor)
import qualified Area.Settings as AS
import Area.State
import Area.Objects.Gate (pos, size)
import Area.Vector (toPos, fromPos, fromPolar, add, sub, angle)
import qualified Area.Objects.User as U


spawnUser :: UserId -> Maybe AreaId -> State -> State
spawnUser uid maybeFromArea state =
    let rGen = mkRandomGen uid $ tickNumber state
        gs = M.elems $ gates state
        (gate, rGen') = choice gs rGen
        minRadius = fromIntegral $ width $ size gate
        maxRadius = (AS.gateFieldRadius . settings) state
        (radius, rGen'') = randomR (minRadius, maxRadius) rGen'
        (spawnAngle, rGen''') = randomR (0, 360) rGen''

        globalPositions = AS.globalPositions $ settings state
        (ang, _) =
            case maybeFromArea of
                Nothing -> randomR (0, 360) rGen'''
                Just (AreaId fromArea) ->
                    let AreaId toArea = areaId state
                        start = globalPositions M.! fromArea
                        end = globalPositions M.! toArea
                    in (angle $ fromPos end `sub` fromPos start, rGen''')
        spawnPos = toPos $ fromPos (pos gate) `add` fromPolar radius spawnAngle
        modUser u = u{U.pos=spawnPos, U.angle=ang}
    in
        if null gs
            then state
            else (userPL uid ^%= modUser) state


updateOwnerName :: State -> Process ()
updateOwnerName state = set key value duration
    where
        key = show $ areaId state
        value = AreaOwner $ ownerName state
        s = settings state
        duration = AS.tickMilliseconds s * AS.syncEveryTick s * durationFactor
