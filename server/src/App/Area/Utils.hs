
module App.Area.Utils where

import Data.Aeson(ToJSON)
import Control.Distributed.Process

import qualified App.Connection as C
import App.Area.Types (Pos)
import App.Area.Vector (len, sub, toVect)


distance :: Pos -> Pos -> Float
distance pos pos' = len $ toVect pos' `sub` toVect pos

getIntervals :: [a] -> [(a, a)]
getIntervals (val:values) = f val values
    where f _ [] = []
          f prev (curr:rest) = (prev, curr) : f curr rest


sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


