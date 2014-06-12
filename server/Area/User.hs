{-# LANGUAGE OverloadedStrings #-}

module Area.User (User(..), tickClientInfo, initClientInfo) where

import Data.Aeson (Value, object, (.=))

import Types (UserId)
import Area.Types (UserName, Pos)
import Area.Action (Active(..), Action(MoveDistance), moveDistance)


instance Active User where

    apply user action@MoveDistance{} ts =
        let (pos', action') = moveDistance action ts
        in (Just user{pos=pos'}, action')

    getActions = actions

    setActions as user = user{actions=as}


data User = User {userId :: UserId,
                  name :: UserName,
                  pos :: Pos,
                  speed :: Int,
                  actions :: [Action]}


tickClientInfo :: User -> Value
tickClientInfo user =
    object ["id" .= userId user,
            "pos" .= pos user,
            "actions" .= actions user]


initClientInfo :: User -> Value
initClientInfo user =
    object ["id" .= userId user,
            "tag" .= ("User" :: String),
            "name" .= name user,
            "pos" .= pos user,
            "actions" .= actions user]


