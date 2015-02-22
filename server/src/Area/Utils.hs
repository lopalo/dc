
module Area.Utils where

import Prelude hiding ((.))
import Control.Category ((.))
import qualified Data.Map.Strict as M

import Data.Lens.Strict ((^$))
import Data.Aeson(ToJSON)
import Control.Distributed.Process hiding (forward)

import qualified Connection as C
import qualified User.External as UE
import Area.Types (Pos(Pos))
import Area.State
import qualified Area.User as U

distance :: Pos -> Pos -> Float
distance (Pos x y) (Pos x' y') =
    sqrt $ fromIntegral (x' - x) ** 2 + fromIntegral (y' - y) ** 2

angle :: Pos -> Pos -> Float
angle (Pos x y) (Pos x' y') =
    degrees $ atan2 (fromIntegral (y' - y)) (fromIntegral (x' - x))

degrees :: Float -> Float
degrees = (/ pi) . (* 180)

sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd = C.broadcastCmd (M.elems cs) ("area." ++ cmd)
    where cs = connectionsL . usersL ^$ state


syncUsers :: State -> Process ()
syncUsers state = mapM_ sync us
    where
        sync usr = let pid = uPids M.! U.userId usr
                   in UE.syncState pid $ U.userArea usr aid
        aid = areaId state
        uPids = userPidsL . usersL ^$ state
        us = M.elems $ usersDataL . usersL ^$ state



