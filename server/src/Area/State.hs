
module Area.State where

import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M

import Data.Lens.Common (lens, Lens, (^%=))

import Connection (Connection)
import Types (UserId, AreaId)
import qualified Area.User as U
import Area.Event (Events)



data Users = Users{connections :: !Connections,
                   userIds :: !UserIds,
                   usersData :: !UsersData}

type Connections = M.Map UserId Connection
type UsersData = M.Map UserId U.User
type UserIds = M.Map Connection UserId

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    users :: !Users,
                    events :: !Events,
                    eventsForBroadcast :: !Events} --TODO: add settings

type State' a = S.State State a

tickNumber' :: Lens State Int
tickNumber' = lens tickNumber (\v s -> s{tickNumber=v})

users' :: Lens State Users
users' = lens users (\v s -> s{users=v})

usersData' :: Lens Users UsersData
usersData' = lens usersData (\v us -> us{usersData=v})

userIds' :: Lens Users UserIds
userIds' = lens userIds (\v us -> us{userIds=v})

connections' :: Lens Users Connections
connections' = lens connections (\v us -> us{connections=v})

events' :: Lens State Events
events' = lens events (\v s -> s{events=v})

eventsForBroadcast' :: Lens State Events
eventsForBroadcast' = lens eventsForBroadcast (\v s -> s{eventsForBroadcast=v})



addUser :: UserId -> Connection -> U.User -> Users -> Users
addUser uid conn user = (usersData' ^%= M.insert uid user)
                      . (userIds' ^%= M.insert conn uid)
                      . (connections' ^%= M.insert uid conn)

deleteUser :: UserId -> Users -> Users
deleteUser uid us = fun us
    where conns = connections us
          fun = (usersData' ^%= M.delete uid)
              . (connections' ^%= M.delete uid)
              . (userIds' ^%= M.delete (conns M.! uid))

