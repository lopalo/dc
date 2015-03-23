{-# LANGUAGE DeriveGeneric #-}

module Area.Action (Active(..), Action(..),
                    moveDistance, moveRoute, burning) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Utils (Ts)
import Area.Utils (getIntervals, angle, fromPoint, toPoint)
import Area.Types (Pos(Pos), Point(Point), Angle)
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
                 startPoint = toPoint $ fromPos action
                 endPoint = toPoint $ toPos action
                 point = middlePoint t startPoint endPoint
             in (fromPoint point, Just action)


moveRoute :: Action -> Ts -> (Pos, Maybe Angle, Maybe Action)
moveRoute action ts =
    if ts >= endTs action
        then (endPos, Nothing, Nothing)
        else let (pos, ang) = reduce $ map toPoint $ positions action
             in (pos, Just ang, Just action)
    where
        endPos = last $ positions action
        t = getT action ts
        reduce :: [Point] -> (Pos, Angle)
        reduce [point, point'] =
            let pos = fromPoint $ middlePoint t point point'
                ang = angle point point'
            in (pos, ang)
        reduce points =
            reduce $ map (uncurry (middlePoint t)) $ getIntervals points


getT :: Action -> Ts -> T
getT action ts =
    let start = startTs action
        td = fromIntegral $ ts - start :: Float
        td' =  fromIntegral $ endTs action - start :: Float
    in td / td'



middlePoint :: T -> Point -> Point -> Point
middlePoint t (Point x y) (Point x' y') =
    let x'' = x + (x' - x) * t
        y'' = y + (y' - y) * t
    in Point x'' y''

