{-# LANGUAGE OverloadedStrings #-}

module Area.User (User(..), tickClientInfo) where

import Data.Aeson (Value, object, (.=))

import Types (UserId)
import Area.Types (UserName, Pos)
import Area.Action (Active(apply), Action(MoveDistance), moveDistance)


instance Active User where
    --TODO: use lenses
    apply user action@MoveDistance{} ts =
        let (pos', action') = moveDistance (pos user) action ts
        in (Just user{pos=pos'}, action')


data User = User {userId :: UserId,
                  name :: UserName,
                  pos :: Pos,
                  speed :: Int,
                  actions :: [Action]}

tickClientInfo :: User -> Value
tickClientInfo user =
    object ["user_id" .= userId user,
            "pos" .= pos user,
            "actions" .= actions user]

