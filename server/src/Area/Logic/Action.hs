{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, RankNTypes #-}

module Area.Logic.Action (
    Active(..), Action(..),
    moveAction, publicAction,
    moveDistance, eternalRotation,
    moveRoute, recovery, moveCircularTrajectory,
    pullingAsteroid, rotation
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)
import Data.Fixed (mod', divMod')
import Data.Sequence (Seq, singleton)
import Control.Monad (foldM)
import Control.Monad.Writer (Writer, writer)

import Data.Aeson (ToJSON, FromJSON)
import Data.Lens.Strict (Lens, lens, (^%=))

import Types (Ts)
import Area.Utils (getIntervals)
import Area.Types
import Area.Logic.Vector (
    Vect, angle, toPos, fromPos,
    fromPolar, mul, sub, add
    )
import Area.Logic.Signal (Signal(MoveAsteroid))


type SignalW = Writer (Seq Signal)


class Active a where

    apply :: a -> Action -> Ts -> SignalW (a, Maybe Action)

    getActions :: a -> [Action]

    setActions :: [Action] -> a -> a

    actionsL :: Lens a [Action]
    actionsL = lens getActions setActions

    applyActions :: Ts -> a -> SignalW a
    applyActions ts active = do
        let actions = getActions active
        (active', actions') <- foldM handleActions (active, []) actions
        return $ setActions actions' active'
        where
            handleActions (act, actions) action = do
                (act', maybeAction) <- apply act action ts
                return $
                    case maybeAction of
                        Nothing -> (act', actions)
                        Just action'-> (act', action' : actions)


data Action
    = MoveDistance {
        startTs :: Ts,
        endTs :: Ts,
        startPos :: Pos,
        endPos :: Pos
        }
    | Rotation {
        startTs :: Ts,
        endTs :: Ts,
        startAngle :: Angle,
        endAngle :: Angle,
        completeSignal :: Signal
        }
    | EternalRotation {
        startTs :: Ts,
        startAngle :: Angle,
        rotSpeed :: Angle --degrees per second
        }
    | MoveCircularTrajectory {
        startTs :: Ts,
        startAngle :: Angle,
        center :: Pos,
        radius :: Int,
        rotSpeed :: Angle
        }
    | MoveRoute {
        startTs :: Ts,
        endTs :: Ts,
        positions :: [Pos]
        }
    | Recovery {
        recoverySpeed :: Float, --units per second
        durabilityAccum :: Float,
        prevTs :: Ts
        }
    | PullingAsteroid {asteroidId :: ObjId}
    deriving (Generic, Typeable, Eq, Ord)

instance Binary Action

instance ToJSON Action

instance FromJSON Action


type T = Float -- in the interval [0, 1]


type ObjectHandler =
    forall o. Object o => o -> Action -> Ts -> SignalW (o, Maybe Action)


publicAction :: Action -> Bool
publicAction MoveDistance{} = True
publicAction Rotation{} = True
publicAction EternalRotation{} = True
publicAction MoveCircularTrajectory{} = True
publicAction MoveRoute{} = True
publicAction _ = False


moveAction :: Action -> Bool
moveAction MoveDistance{} = True
moveAction Rotation{} = True
moveAction EternalRotation{} = True
moveAction MoveCircularTrajectory{} = True
moveAction MoveRoute{} = True
moveAction _ = False


recovery :: Destroyable d => d -> Action -> Ts -> SignalW (d, Maybe Action)
recovery obj action ts = do
    let Recovery{recoverySpeed=speed, durabilityAccum=accum} = action
        dSeconds = (fromIntegral $ ts - prevTs action) / 1000
        accum' = dSeconds * speed + accum
        (durabilityDelta, accum'') = accum' `divMod'` 1
        action' = action{durabilityAccum=accum'', prevTs=ts}
        maxDur = getMaxDurability obj
        update = min maxDur . (durabilityDelta +)
        obj' = (durabilityL ^%= update) obj
        maybeAction =
            if getDurability obj' < maxDur
                then Just action'
                else Nothing
    return (obj', maybeAction)


moveDistance :: ObjectHandler
moveDistance obj action ts = do
    let t = getT action ts
        startPoint = fromPos $ startPos action
        endPoint = fromPos $ endPos action
        point = middlePoint t startPoint endPoint
        (pos, maybeAction) =
            if ts >= endTs action
                then (endPos action, Nothing)
                else (toPos point, Just action)
    return (setPos pos obj, maybeAction)


rotation :: ObjectHandler
rotation obj action ts = do
    let t = getT action ts
        start = startAngle action
        end = endAngle action
    (ang, maybeAction) <-
        if ts >= endTs action
            then do
                putSignal $ completeSignal action
                return (end `mod'` 360, Nothing)
            else return ((end - start) * t + start, Just action)
    return (setAngle ang obj, maybeAction)


eternalRotation :: ObjectHandler
eternalRotation obj action ts = do
    let angleDelta = rotSpeed action * secondsDelta action ts
        ang = (startAngle action + angleDelta) `mod'` 360
    return (setAngle ang obj, Just action)


moveCircularTrajectory :: ObjectHandler
moveCircularTrajectory obj action ts = do
    let angleDelta = rotSpeed action * secondsDelta action ts
        newAngle = (startAngle action + angleDelta) `mod'` 360
        rotVect = fromPolar (fromIntegral (radius action)) newAngle
        pos = toPos $ fromPos (center action) `add` rotVect
    return (setPos pos obj, Just action)


moveRoute :: ObjectHandler
moveRoute obj action ts = do
    let (pos, ang) = reduce $ map fromPos $ positions action
        (pos', maybeAngle, maybeAction) =
            if ts >= endTs action
                then (endP, Nothing, Nothing)
                else (pos, Just ang, Just action)
        obj' = setPos pos' obj
        obj'' = setAngle (fromMaybe (getAngle obj') maybeAngle) obj'
    return (obj'', maybeAction)
    where
        endP = last $ positions action
        t = getT action ts
        --De Casteljau's algorithm
        reduce :: [Vect] -> (Pos, Angle)
        reduce [point, point'] =
            let pos = toPos $ middlePoint t point point'
                ang = angle $ point' `sub` point
            in (pos, ang)
        reduce points =
            reduce $ map (uncurry (middlePoint t)) $ getIntervals points


pullingAsteroid :: ObjectHandler
pullingAsteroid obj action _ = do
    putSignal $ MoveAsteroid (asteroidId action) (getPos obj)
    return (obj, Just action)


secondsDelta :: Action -> Ts -> Float
secondsDelta action ts = (fromIntegral $ ts - startTs action) / 1000


getT :: Action -> Ts -> T
getT action ts =
    let start = startTs action
        td = fromIntegral $ ts - start :: Float
        td' =  fromIntegral $ endTs action - start :: Float
    in td / td'


middlePoint :: T -> Vect -> Vect -> Vect
middlePoint t v v' = v' `sub` v `mul` t `add` v


putSignal :: Signal -> SignalW ()
putSignal sig = writer ((), singleton sig)
