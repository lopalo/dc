{-# LANGUAGE OverloadedStrings #-}

module Area.Objects.User where

import Data.Aeson (object, (.=))
import Data.Lens.Strict (Lens, lens)

import Types (UserId, UserName, UserMonitorRef, UserPid, Size(Size), Ts)
import WS.Connection (Connection)
import qualified User.UserArea as UA
import Area.Types (
    Positioned(..), Object(..), Destroyable(..),
    Pos, Angle, ObjId(UId),
    )
import Area.Action (
    Active(..),
    Action(Rotation, MoveRoute, Recovery, PullingAsteroid),
    rotation, moveRoute, recovery, publicAction, pullingAsteroid
    )
import Area.Collision (Collidable(collider), Collider(Circular))


data User = User {
    userId :: !UserId,
    connection :: !Connection,
    pid :: !UserPid,
    monitorRef :: !UserMonitorRef,
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
    lastAttacker :: Maybe UserId,
    nextShootTs :: Ts
    }


instance Positioned User where

    getPos = pos

    setPos p user = user{pos=p}


instance Object User where

    objId = UId . userId

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

    apply user action@Rotation{} = rotation user action
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


connectionL :: Lens User Connection
connectionL = lens connection (\v s -> s{connection=v})


killsL :: Lens User Int
killsL = lens kills (\v s -> s{kills=v})


deathsL :: Lens User Int
deathsL = lens deaths (\v s -> s{deaths=v})


lastAttackerL :: Lens User (Maybe UserId)
lastAttackerL = lens lastAttacker (\v s -> s{lastAttacker=v})


userArea :: User -> UA.UserArea
userArea user = UA.UserArea{
    UA.userId=userId user,
    UA.name=name user,
    UA.speed=speed user,
    UA.maxDurability=maxDurability user,
    UA.durability=durability user,
    UA.size=size user,
    UA.kills=kills user,
    UA.deaths=deaths user
    }




