{-# LANGUAGE OverloadedStrings #-}

module Area.User (User(..), tickClientInfo, initClientInfo) where

import Data.Aeson (Value, object, (.=))

import Types (UserId)
import Area.Types (UserName, Pos)
import Area.Action (Active(..), Action(MoveDistance, Burning),
                    moveDistance, burning)
import Area.Event (Event(DeleteUser))


instance Active User where

    apply user action@MoveDistance{} ts =
        let (pos', action') = moveDistance action ts
        in (user{pos=pos'}, action', [])
    apply user@User{durability=d} action@Burning{} ts =
        let (damage, action') = burning action ts
            durability' = d - damage
            user' = user{durability=durability'}
        in if durability' <= 0
           then (user, action', [DeleteUser $ userId user])
           else (user', action', [])


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


