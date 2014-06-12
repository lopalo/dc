{-# LANGUAGE DeriveGeneric #-}

module Area.Action (Active(..), Action(..), moveDistance) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Area.Types (Pos(Pos), Ts)


class Active a where

    apply :: a -> Action -> Ts -> (Maybe a, Maybe Action)

    getActions :: a -> [Action]

    setActions :: [Action] -> a -> a

    applyActions :: Ts -> a -> Maybe a
    applyActions ts active =
        let actions = getActions active
            (res, actions') = foldr handleActions (Just active, []) actions
        in fmap (setActions actions') res
        where
            handleActions _ (Nothing, _) = (Nothing, [])
            handleActions action (Just act, actions) =
                case apply act action ts of
                    (res, Nothing) -> (res, actions)
                    (res, Just action') -> (res, action':actions)


data Action = MoveDistance{startTs :: Ts,
                           endTs :: Ts,
                           from :: Pos,
                           to :: Pos}
            | Shoot{startTs :: Ts,
                    endTs :: Ts,
                    target :: Pos}
            deriving (Generic)

instance ToJSON Action


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




