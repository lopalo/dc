{-# LANGUAGE OverloadedStrings #-}

module Area.Objects.User where

import Data.Aeson (object, (.=))
import Data.Lens.Strict (Lens, lens)

import Types (UserId, UserName, Size(Size))
import qualified User.External as UE
import Area.Types (Object(..), Destroyable(..), Pos, Angle, ObjId(UId))
import Area.Action (
    Active(..), Action(MoveRoute, Recovery, PullingAsteroid),
    moveRoute, recovery, publicAction, pullingAsteroid
    )
import Area.Collision (Collidable(collider), Collider(Circular))


data User = User {
    userId :: !UserId,
    name :: !UserName,
    pos :: !Pos,
    angle :: !Angle,
    speed :: !Int, --units per second
    maxDurability :: !Int,
    durability :: !Int,
    actions :: ![Action],
    size :: !Size,
    kills :: !Int,
    deaths :: !Int,
    lastAttacker :: Maybe UserId
    }


instance Object User where

    objId = UId . userId

    getPos = pos

    setPos p user = user{pos=p}

    getAngle = angle

    setAngle a user = user{angle=a}

    initClientInfo user =
        object [
            "id" .= userId user,
            "tag" .= ("User" :: String),
            "name" .= name user,
            "max-durability" .= maxDurability user,
            "durability" .= durability user,
            "speed" .= speed user,
            "angle" .= angle user,
            "pos" .= pos user,
            "size" .= size user,
            "kills" .= kills user,
            "deaths" .= deaths user
            ]

    tickClientInfo user =
        object [
            "id" .= userId user,
            "pos" .= pos user,
            "angle" .= angle user,
            "durability" .= durability user,
            "actions" .= filter publicAction (actions user),
            "kills" .= kills user,
            "deaths" .= deaths user
            ]


instance Active User where

    apply user action@MoveRoute{} = moveRoute user action
    apply user action@Recovery{} = recovery user action
    apply user action@PullingAsteroid{} = pullingAsteroid user action

    getActions = actions

    setActions as user = user{actions=as}


instance Destroyable User where

    getMaxDurability = maxDurability

    setMaxDurability d user = user{maxDurability=d}

    getDurability = durability

    setDurability d user = user{durability=d}


instance Collidable User where

    collider user = Circular (objId user) (getPos user) radius
        where
            Size w h = size user
            radius = (w + h) `quot` 4


killsL :: Lens User Int
killsL = lens kills (\v s -> s{kills=v})


deathsL :: Lens User Int
deathsL = lens deaths (\v s -> s{deaths=v})


lastAttackerL :: Lens User (Maybe UserId)
lastAttackerL = lens lastAttacker (\v s -> s{lastAttacker=v})


userArea :: User -> UE.UserArea
userArea user = UE.UserArea{
    UE.userId=userId user,
    UE.name=name user,
    UE.speed=speed user,
    UE.maxDurability=maxDurability user,
    UE.durability=durability user,
    UE.size=size user,
    UE.kills=kills user,
    UE.deaths=deaths user
    }




