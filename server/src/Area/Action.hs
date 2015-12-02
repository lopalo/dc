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

import Data.Aeson (ToJSON, FromJSON)
import Data.Lens.Strict (Lens, lens, (^+=), (^%=))

import Utils (Ts)
import Area.Utils (getIntervals)
import Area.Types
import Area.Vector (Vect, angle, fromVect, toVect,
                    fromLenAndAngle, mul, sub, add)
import Area.Signal (Signals)


class Active a where

    apply :: a -> Action -> Time -> (a, Maybe Action, Signals)

    getActions :: a -> [Action]

    setActions :: [Action] -> a -> a

    actionsL :: Lens a [Action]
    actionsL = lens getActions setActions


    applyActions :: Time -> a -> (a, Signals)
    applyActions time active =
        let actions = getActions active
            res = foldr handleActions (active, [], []) actions
            (active', actions', signals) = res
        in (setActions actions' active', signals)
        where
            handleActions action (act, actions, signals) =
                let (act', maybeAction, newSignals) = apply act action time
                    --TODO: use Writer monad
                    signals' = newSignals ++ signals
                in case maybeAction of
                    Nothing -> (act', actions, signals')
                    Just action'-> (act', action' : actions, signals')


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
                     o -> Action -> Time -> (o, Maybe Action, Signals)


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


recovery :: Destroyable d => d -> Action -> Time -> (d, Maybe Action, Signals)
recovery obj action time =
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
    in (obj', maybeAction, [])


moveDistance :: ObjectHandler
moveDistance obj action Time{timestamp=ts} =
    let (pos, maybeAction) = if ts >= endTs action
            then (toPos action, Nothing)
            else let t = getT action ts
                     startPoint = toVect $ fromPos action
                     endPoint = toVect $ toPos action
                     point = middlePoint t startPoint endPoint
                 in (fromVect point, Just action)
    in (setPos pos obj, maybeAction, [])


eternalRotation :: ObjectHandler
eternalRotation obj action@EternalRotation{rotSpeed=speed} time =
    let angle = getAngle obj
        angleDelta = speed * secondsDelta time
        angle' = (angle + angleDelta) `mod'` 360
    in (setAngle angle' obj, Just action, [])


moveCircularTrajectory :: ObjectHandler
moveCircularTrajectory obj action time =
    let angleDelta = rotSpeed action * secondsDelta time
        newAngle = (curAngle action + angleDelta) `mod'` 360
        action' = action{curAngle=newAngle}
        rotVect = fromLenAndAngle (fromIntegral (radius action)) newAngle
        pos = fromVect $ toVect (center action) `add` rotVect
    in (setPos pos obj, Just action', [])


moveRoute :: ObjectHandler
moveRoute obj action Time{timestamp=ts} =
    let (pos, maybeAngle, maybeAction) = if ts >= endTs action
            then (endPos, Nothing, Nothing)
            else let (pos, ang) = reduce $ map toVect $ positions action
                 in (pos, Just ang, Just action)
        obj' = setPos pos obj
        obj'' = setAngle (fromMaybe (getAngle obj') maybeAngle) obj'
    in (obj'', maybeAction, [])
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
