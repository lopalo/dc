{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, RankNTypes #-}

module Area.Action (Active(..), Action(..), Time(..),
                    moveAction, publicAction,
                    moveDistance, eternalRotation,
                    moveRoute, recovery, moveCircularTrajectory) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Data.Maybe (fromMaybe)
import Data.Fixed (mod', divMod')
import Data.Set (Set)
import Control.Monad (foldM)
import Control.Monad.Writer (Writer)

import Data.Aeson (ToJSON, FromJSON)
import Data.Lens.Strict (Lens, lens, (^+=), (^%=))

import Utils (Ts)
import Area.Utils (getIntervals)
import Area.Types
import Area.Vector (Vect, angle, fromVect, toVect,
                    fromLenAndAngle, mul, sub, add)
import Area.Signal (Signal)


type SignalW = Writer (Set Signal)

class Active a where

    apply :: a -> Action -> Time -> SignalW (a, Maybe Action)

    getActions :: a -> [Action]

    setActions :: [Action] -> a -> a

    actionsL :: Lens a [Action]
    actionsL = lens getActions setActions


    applyActions :: Time -> a -> SignalW a
    applyActions time active = do
        let actions = getActions active
        (active', actions') <- foldM handleActions (active, []) actions
        return $ setActions actions' active'
        where
            handleActions (act, actions) action = do
                (act', maybeAction) <- apply act action time
                return $ case maybeAction of
                    Nothing -> (act', actions)
                    Just action'-> (act', action' : actions)


data Time = Time {timestamp :: Ts, timeDelta :: Ts}


data Action = MoveDistance {startTs :: Ts,
                            endTs :: Ts,
                            fromPos :: Pos,
                            toPos :: Pos}
            | Rotation {startTs :: Ts,
                        endTs :: Ts,
                        from :: Angle,
                        to :: Angle}
            | EternalRotation {rotSpeed :: Angle} --degrees per second
            | MoveCircularTrajectory {center :: Pos,
                                      radius :: Int,
                                      rotSpeed :: Angle,
                                      curAngle :: Angle}
            | MoveRoute {startTs :: Ts,
                         endTs :: Ts,
                         positions :: [Pos]}
            | Recovery {recoverySpeed :: Float, --units per second
                        durabilityAccum :: Float}
            deriving (Generic, Typeable)

instance Binary Action

instance ToJSON Action
instance FromJSON Action


type T = Float -- in the interval [0, 1]
type ObjectHandler = forall o. Object o =>
                     o -> Action -> Time -> SignalW (o, Maybe Action)


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


recovery :: Destroyable d => d -> Action -> Time -> SignalW (d, Maybe Action)
recovery obj action time = do
    let Recovery{recoverySpeed=speed, durabilityAccum=accum} = action
        accum' = secondsDelta time * speed + accum
        (durabilityDelta, accum'') = accum' `divMod'` 1
        action' = action{durabilityAccum=accum''}
        maxDur = getMaxDurability obj
        update = min maxDur . (durabilityDelta +)
        obj' = (durabilityL ^%= update) obj
        maybeAction = if getDurability obj' < maxDur
            then Just action'
            else Nothing
    return (obj', maybeAction)


moveDistance :: ObjectHandler
moveDistance obj action Time{timestamp=ts} = do
    let t = getT action ts
        startPoint = toVect $ fromPos action
        endPoint = toVect $ toPos action
        point = middlePoint t startPoint endPoint
        (pos, maybeAction) = if ts >= endTs action
            then (toPos action, Nothing)
            else (fromVect point, Just action)
    return (setPos pos obj, maybeAction)


eternalRotation :: ObjectHandler
eternalRotation obj action@EternalRotation{rotSpeed=speed} time = do
    let angle = getAngle obj
        angleDelta = speed * secondsDelta time
        angle' = (angle + angleDelta) `mod'` 360
    return (setAngle angle' obj, Just action)


moveCircularTrajectory :: ObjectHandler
moveCircularTrajectory obj action time = do
    let angleDelta = rotSpeed action * secondsDelta time
        newAngle = (curAngle action + angleDelta) `mod'` 360
        action' = action{curAngle=newAngle}
        rotVect = fromLenAndAngle (fromIntegral (radius action)) newAngle
        pos = fromVect $ toVect (center action) `add` rotVect
    return (setPos pos obj, Just action')


moveRoute :: ObjectHandler
moveRoute obj action Time{timestamp=ts} = do
    let (pos, ang) = reduce $ map toVect $ positions action
        (pos', maybeAngle, maybeAction) = if ts >= endTs action
            then (endPos, Nothing, Nothing)
            else (pos, Just ang, Just action)
        obj' = setPos pos' obj
        obj'' = setAngle (fromMaybe (getAngle obj') maybeAngle) obj'
    return (obj'', maybeAction)
    where
        endPos = last $ positions action
        t = getT action ts
        --De Casteljau's algorithm
        reduce :: [Vect] -> (Pos, Angle)
        reduce [point, point'] =
            let pos = fromVect $ middlePoint t point point'
                ang = angle $ point' `sub` point
            in (pos, ang)
        reduce points =
            reduce $ map (uncurry (middlePoint t)) $ getIntervals points


secondsDelta :: Time -> Float
secondsDelta = (/ 1000) . fromIntegral . timeDelta


getT :: Action -> Ts -> T
getT action ts =
    let start = startTs action
        td = fromIntegral $ ts - start :: Float
        td' =  fromIntegral $ endTs action - start :: Float
    in td / td'



middlePoint :: T -> Vect -> Vect -> Vect
middlePoint t v v' = v' `sub` v `mul` t `add` v
