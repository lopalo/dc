
module Area.Utils where

import Data.List (foldl')
import qualified Data.Map.Strict as M

import Data.Aeson (ToJSON)
import Control.Distributed.Process

import qualified WS.Connection as C
import Area.Types (Pos(Pos), Positioned(getPos))
import Area.Vector (len, sub, fromPos)


type Groups g = M.Map Pos g


distance :: Pos -> Pos -> Float
distance pos pos' = len $ fromPos pos' `sub` fromPos pos


getIntervals :: [a] -> [(a, a)]
getIntervals (val:values) = f val values
    where
        f _ [] = []
        f prev (curr:rest) = (prev, curr) : f curr rest


sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


groupByCells ::
    Positioned p => Int -> g -> (g -> p -> g) -> [p] -> Groups g -> Groups g
groupByCells cellSize defaultGroup reducer ps groups =
    foldl' groupReducer groups ps
    where
        cf = (`quot` cellSize)
        groupReducer groups' p =
            let Pos x y = getPos p
                pos' = Pos (cf x) (cf y)
                group = M.findWithDefault defaultGroup pos' groups'
                group' = reducer group p
            in M.insert pos' group' groups'


groupCells :: Groups g -> [(g, [g])]
groupCells groups = M.foldlWithKey' reducer [] groups
    where
        reducer acc (Pos x y) center =
            let neighbours = [group | Just group <- map neighbour shifts]
                neighbour (x', y') =
                    let pos' = Pos (x + x') (y + y')
                    in M.lookup pos' groups
            in (center, center : neighbours) : acc
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


