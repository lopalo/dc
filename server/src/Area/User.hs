{-# LANGUAGE OverloadedStrings #-}

module Area.User where

import Data.Aeson (Value, object, (.=))
import Data.Lens.Strict (lens, Lens, (^-=))

import Types (UserId, UserName, AreaId)
import qualified User.External as UE
import Area.Types (Object(..), Destroyable(..), Pos, ObjId(UId))
import Area.Action (Active(..), Action(MoveDistance, Burning),
                    moveDistance, burning)


instance Object User where
    objId = UId . userId

instance Destroyable User where

    objDurability = durability

instance Active User where

    apply user action@MoveDistance{} ts =
        let (pos', action') = moveDistance action ts
        in (user{pos=pos'}, action', [])
    apply user action@Burning{} ts =
        let (damage, action') = burning action ts
            user' = (durabilityL ^-= damage) user
        in (user', action', [])


    getActions = actions

    setActions as user = user{actions=as}


data User = User {userId :: !UserId,
                  name :: !UserName,
                  pos :: !Pos,
                  angle :: !Float, --degrees
                  speed :: !Int, --units per second
                  durability :: !Int,
                  actions :: ![Action]}

durabilityL :: Lens User Int
durabilityL = lens durability (\v user -> user{durability=v})

actionsL :: Lens User [Action]
actionsL = lens actions (\v user -> user{actions=v})


userArea :: User -> AreaId -> UE.UserArea
userArea user area =
    UE.UserArea{UE.userId=userId user,
                UE.name=name user,
                UE.area=area,
                UE.speed=speed user,
                UE.durability=durability user}



tickClientInfo :: User -> Value
tickClientInfo user =
    object ["id" .= userId user,
            "pos" .= pos user,
            "angle" .= angle user,
            "durability" .= durability user,
            "actions" .= actions user]


initClientInfo :: User -> Value
initClientInfo user =
    object ["id" .= userId user,
            "tag" .= ("User" :: String),
            "name" .= name user,
            "durability" .= durability user,
            "speed" .= speed user,
            "angle" .= angle user,
            "pos" .= pos user]


