
module Area.Utils where

import Data.Aeson (ToJSON)
import Control.Distributed.Process

import qualified WS.Connection as C
import Area.Types (Pos)
import Area.Vector (len, sub, fromPos)


distance :: Pos -> Pos -> Float
distance pos pos' = len $ fromPos pos' `sub` fromPos pos


getIntervals :: [a] -> [(a, a)]
getIntervals (val:values) = f val values
    where
        f _ [] = []
        f prev (curr:rest) = (prev, curr) : f curr rest


sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


