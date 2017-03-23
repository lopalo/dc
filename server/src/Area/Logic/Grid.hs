
module Area.Logic.Grid where

import Data.List (foldl')
import qualified Data.Map.Strict as M

import Area.Types (Pos(Pos), Positioned(getPos))


type Groups g = M.Map Pos g


cellPos :: Int -> Pos -> Pos
cellPos cellSize (Pos x y) = Pos (cf x) (cf y)
    where cf = (`quot` cellSize)


groupByCells ::
    Positioned p => Int -> g -> (g -> p -> g) -> [p] -> Groups g -> Groups g
groupByCells cellSize defaultGroup reducer ps groups =
    foldl' groupReducer groups ps
    where
        groupReducer groups' p =
            let pos' = cellPos cellSize $ getPos p
                group = M.findWithDefault defaultGroup pos' groups'
                group' = reducer group p
            in M.insert pos' group' groups'


formZones :: Groups g -> M.Map Pos [g]
formZones groups = M.mapWithKey m groups
    where
        m (Pos x y) center =
            let neighbours = [group | Just group <- map neighbour shifts]
                neighbour (x', y') =
                    let pos' = Pos (x + x') (y + y')
                    in M.lookup pos' groups
            in center : neighbours
        shifts = [
            (0, 1),
            (1, 1),
            (1, 0),
            (1, -1),
            (0, -1),
            (-1, -1),
            (-1, 0),
            (-1, 1)
            ]


