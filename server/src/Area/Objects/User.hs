{-# LANGUAGE OverloadedStrings #-}

module Area.Objects.User where

import Data.Maybe (fromMaybe)

import Data.Aeson (Value, object, (.=))
import Data.Lens.Strict ((^+=), (^%=))

import Types (UserId, UserName)
import qualified User.External as UE
import Area.Types (Object(..), Destroyable(..), Pos, Angle, ObjId(UId))
import Area.Action (Active(..), Action(MoveRoute, Recovery),
                    moveRoute, recovery, publicAction)


data User = User {userId :: !UserId,
                  name :: !UserName,
                  pos :: !Pos,
                  angle :: !Angle,
                  speed :: !Int, --units per second
                  maxDurability :: !Int,
                  durability :: !Int,
                  actions :: ![Action]}


instance Object User where
    objId = UId . userId

    getPos = pos
    setPos p user = user{pos=p}

    getAngle = angle
    setAngle a user = user{angle=a}


    initClientInfo user =
        object ["id" .= userId user,
                "tag" .= ("User" :: String),
                "name" .= name user,
                "max-durability" .= maxDurability user,
                "durability" .= durability user,
                "speed" .= speed user,
                "angle" .= angle user,
                "pos" .= pos user]

    tickClientInfo user =
        object ["id" .= userId user,
                "pos" .= pos user,
                "angle" .= angle user,
                "durability" .= durability user,
                "actions" .= filter publicAction (actions user)]



instance Active User where

    apply user action@MoveRoute{} = moveRoute user action
    apply user action@Recovery{} = recovery user action


    getActions = actions

    setActions as user = user{actions=as}


instance Destroyable User where

    getMaxDurability = maxDurability
    setMaxDurability d user = user{maxDurability=d}

    getDurability = durability
    setDurability d user = user{durability=d}


userArea :: User -> UE.UserArea
userArea user =
    UE.UserArea{UE.userId=userId user,
                UE.name=name user,
                UE.speed=speed user,
                UE.maxDurability=maxDurability user,
                UE.durability=durability user}




