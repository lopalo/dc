
module Area.Utils (distance, sendCmd, broadcastCmd, broadcastCmd') where

import Control.Monad.State (get, lift)
import qualified Data.Map as M

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
broadcastCmd state cmd =
    C.broadcastCmd (M.elems (connections state)) ("area." ++ cmd)


broadcastCmd' :: ToJSON a => String -> a -> State' ()
broadcastCmd' cmd body = do
    state <- get
    lift $ broadcastCmd state cmd body

