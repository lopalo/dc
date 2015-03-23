
module Area.Utils where

import Prelude hiding ((.))
import Control.Category ((.))

import Data.Aeson(ToJSON)
import Control.Distributed.Process

import qualified Connection as C
import Area.Types (Pos(Pos), Point(Point), Angle)


distance :: Pos -> Pos -> Float
distance (Pos x y) (Pos x' y') =
    sqrt $ fromIntegral (x' - x) ** 2 + fromIntegral (y' - y) ** 2

angle :: Point -> Point -> Angle
angle (Point x y) (Point x' y') = degrees $ atan2 (y' - y) (x' - x)

degrees :: Float -> Float
degrees = (/ pi) . (* 180)


toPoint :: Pos -> Point
toPoint (Pos x y) = Point (fromIntegral x) (fromIntegral y)

fromPoint :: Point -> Pos
fromPoint (Point x y) = Pos (round x) (round y)


getIntervals :: [a] -> [(a, a)]
getIntervals (val:values) = f val values
    where f _ [] = []
          f prev (curr:rest) = (prev, curr) : f curr rest


sendCmd :: ToJSON a => C.Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


