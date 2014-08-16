
module Area.Utils (distance, sendCmd, broadcastCmd) where

import Prelude hiding ((.))
import Control.Category ((.))
import qualified Data.Map.Strict as M

import Data.Lens.Common ((^$))
import Data.Aeson(ToJSON)
import Control.Distributed.Process hiding (forward)

import qualified Connection as C
import Area.Types (Pos(Pos))
import Area.State

distance :: Pos -> Pos -> Float
distance (Pos x y) (Pos x' y') =
    sqrt $ fromIntegral (x' - x) ** 2 + fromIntegral (y' - y) ** 2

sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd = C.broadcastCmd (M.elems cs) ("area." ++ cmd)
    where cs = connections' . users' ^$ state

