{-# LANGUAGE DeriveGeneric #-}

module Area.Action (Active(..), Action(..), moveDistance, burning) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Area.Types (Pos(Pos), Ts)
import Area.Event (Events)


class Active a where

    apply :: a -> Action -> Ts -> (a, Maybe Action, Events)

    getActions :: a -> [Action]

    setActions :: [Action] -> a -> a

    applyActions :: Ts -> a -> (a, Events)
    applyActions ts active =
        let actions = getActions active
            res = foldr handleActions (active, [], []) actions
            (active', actions', events) = res
        in (setActions actions' active', events)
        where
            handleActions action (act, actions, events) =
                let (act', maybeAction, newEvents) = apply act action ts
                    events' = newEvents ++ events
                in case maybeAction of
                    Nothing -> (act', actions, events')
                    Just action'-> (act', action':actions, events')


data Action = MoveDistance{startTs :: Ts,
                           endTs :: Ts,
                           from :: Pos,
                           to :: Pos}
            | Burning{damageSpeed :: Float, --units per second
                      previousTs :: Ts}
            deriving (Generic)

instance ToJSON Action


burning :: Action -> Ts -> (Int, Maybe Action)
burning action@Burning{previousTs=pts, damageSpeed=speed} ts =
    let damage = round $ speed * fromIntegral (ts - pts) / 1000
    in
        if damage > 1
           then(damage, Just action{previousTs=ts})
           else (0, Just action)

moveDistance :: Action -> Ts -> (Pos, Maybe Action)
moveDistance action ts =
    if ts >= endTs action
        then (to action, Nothing)
        else let t = getT action ts
                 Pos fx fy = from action
                 Pos tx ty = to action
                 x' = fx + round (fromIntegral (tx - fx) * t)
                 y' = fy + round (fromIntegral (ty - fy) * t)
             in (Pos x' y', Just action)

getT :: Action -> Ts -> Float
getT action ts =
    let start = startTs action
        td = fromIntegral $ ts - start :: Float
        td' =  fromIntegral $ endTs action - start :: Float
    in td / td'




