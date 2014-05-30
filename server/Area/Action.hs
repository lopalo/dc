{-# LANGUAGE DeriveGeneric #-}

module Area.Action (Active(apply), Action(..), moveDistance) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

import Area.Types (Pos, Ts)


class Active a where

    apply :: a -> Action -> Ts -> (Maybe a, Maybe Action)


data Action = MoveDistance{startTs :: Ts,
                           endTs :: Ts,
                           from :: Pos,
                           to :: Pos}
            | Shoot{startTs :: Ts,
                    endTs :: Ts,
                    target :: Pos}
            deriving (Generic)

instance ToJSON Action


moveDistance :: Pos -> Action -> Ts -> (Pos, Maybe Action)
moveDistance pos action ts = (pos, Just action) --TODO

