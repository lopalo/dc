{-# LANGUAGE DeriveGeneric #-}

module Area.Action (Active(..), Action(..),
                    moveDistance, moveRoute, burning) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Utils (Ts)
import Area.Utils (getIntervals)
import Area.Types (Pos, Angle)
import Area.Vector (Vect, angle, fromVect, toVect, mul, sub, add)
import Area.Signal (Signals)


class Active a where

    apply :: a -> Action -> Ts -> (a, Maybe Action, Signals)

    getActions :: a -> [Action]

    setActions :: [Action] -> a -> a

    applyActions :: Ts -> a -> (a, Signals)
    applyActions ts active =
        let actions = getActions active
            res = foldr handleActions (active, [], []) actions
            (active', actions', signals) = res
        in (setActions actions' active', signals)
        where
            handleActions action (act, actions, signals) =
                let (act', maybeAction, newSignals) = apply act action ts
                    --TODO: use Writer monad
                    signals' = newSignals ++ signals
                in case maybeAction of
                    Nothing -> (act', actions, signals')
                    Just action'-> (act', action' : actions, signals')


data Action = MoveDistance {startTs :: Ts,
                            endTs :: Ts,
                            fromPos :: Pos,
                            toPos :: Pos}
            | Rotation {startTs :: Ts,
                        endTs :: Ts,
                        from :: Angle,
                        to :: Angle}
            | MoveRoute {startTs :: Ts,
                         endTs :: Ts,
                         positions :: [Pos]}
            | Burning {damageSpeed :: Float, --units per second
                       previousTs :: Ts}
            deriving (Generic)

instance ToJSON Action


type T = Float -- in the interval [0, 1]

burning :: Action -> Ts -> (Int, Maybe Action)
burning action@Burning{previousTs=pts, damageSpeed=speed} ts =
    let damage = round $ speed * fromIntegral (ts - pts) / 1000
    in
        if damage > 1
           then (damage, Just action{previousTs=ts})
           else (0, Just action)


moveDistance :: Action -> Ts -> (Pos, Maybe Action)
moveDistance action ts =
    if ts >= endTs action
        then (toPos action, Nothing)
        else let t = getT action ts
                 startPoint = toVect $ fromPos action
                 endPoint = toVect $ toPos action
                 point = middlePoint t startPoint endPoint
             in (fromVect point, Just action)


moveRoute :: Action -> Ts -> (Pos, Maybe Angle, Maybe Action)
moveRoute action ts =
    if ts >= endTs action
        then (endPos, Nothing, Nothing)
        else let (pos, ang) = reduce $ map toVect $ positions action
             in (pos, Just ang, Just action)
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


getT :: Action -> Ts -> T
getT action ts =
    let start = startTs action
        td = fromIntegral $ ts - start :: Float
        td' =  fromIntegral $ endTs action - start :: Float
    in td / td'



middlePoint :: T -> Vect -> Vect -> Vect
middlePoint t v v' = v' `sub` v `mul` t `add` v
