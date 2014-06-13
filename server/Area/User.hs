{-# LANGUAGE OverloadedStrings #-}

module Area.User (User(..), tickClientInfo, initClientInfo) where

import Data.Aeson (Value, object, (.=))

import Types (UserId)
import Area.Types (UserName, Pos)
import Area.Action (Active(..), Action(MoveDistance, Burning),
                    moveDistance, burning)


instance Active User where

    apply user action@MoveDistance{} ts =
        let (pos', action') = moveDistance action ts
        in (Just user{pos=pos'}, action')
    apply user@User{durability=d} action@Burning{} ts =
        let (damage, action') = burning action ts
            durability' = d - damage
        in if durability' <= 0
           then (Nothing, action')
           else (Just user{durability=durability'}, action')


    getActions = actions

    setActions as user = user{actions=as}


data User = User {userId :: UserId,
                  name :: UserName,
                  pos :: Pos,
                  speed :: Int, --units per second
                  durability :: Int,
                  actions :: [Action]}


tickClientInfo :: User -> Value
tickClientInfo user =
    object ["id" .= userId user,
            "pos" .= pos user,
            "durability" .= durability user,
            "actions" .= actions user]


initClientInfo :: User -> Value
initClientInfo user =
    object ["id" .= userId user,
            "tag" .= ("User" :: String),
            "name" .= name user,
            "pos" .= pos user,
            "actions" .= actions user]


