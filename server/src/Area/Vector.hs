
module Area.Vector where


import Area.Types (Pos(Pos), Angle)

data Vect = Vect Float Float deriving (Show)


degrees :: Float -> Float
degrees = (/ pi) . (* 180)

radians :: Float -> Float
radians = (pi *) . (/ 180)


add :: Vect -> Vect -> Vect
add(Vect x y) (Vect x' y') = Vect (x + x') (y + y')

sub :: Vect -> Vect -> Vect
sub (Vect x y) (Vect x' y') = Vect (x - x') (y - y')

mul :: Vect -> Float -> Vect
mul (Vect x y) t = Vect (x * t) (y * t)

divide :: Vect -> Float -> Vect
divide (Vect x y) t = Vect (x / t) (y / t)

lenSqr :: Vect -> Float
lenSqr (Vect x y) = x ** 2 + y ** 2

len :: Vect -> Float
len = sqrt . lenSqr

angle :: Vect -> Angle
angle (Vect x y) = degrees $ atan2 y x

dot :: Vect -> Vect -> Float
dot (Vect x y) (Vect x' y') = x * x' + y * y'

fromPos :: Pos -> Vect
fromPos (Pos x y) = Vect (fromIntegral x) (fromIntegral y)

toPos :: Vect -> Pos
toPos (Vect x y) = Pos (round x) (round y)

fromPolar :: Float -> Angle -> Vect
fromPolar radius degr =
    let rad = radians degr
        x = radius * cos rad
        y = radius * sin rad
    in Vect x y


projectionToSegment :: Pos -> Pos -> Pos -> Vect
projectionToSegment start end point =
    let p = fromPos point
        s = fromPos start
        e = fromPos end
        sp = p `sub` s
        se = e `sub` s
    in case sp `dot` se / lenSqr se of
            t | start == end -> s
              | t < 0 -> s
              | t > 1 -> e
              | otherwise -> se `mul` t `add` s


distanceToSegment :: Pos -> Pos -> Pos -> Float
distanceToSegment start end point =
    let p = fromPos point
        v = projectionToSegment start end point `sub` p
    in len v

