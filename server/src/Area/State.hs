
module Area.State where

import Prelude hiding ((.))
import Control.Category ((.))
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M

import Data.Lens.Common (lens, Lens, (^%=))

import Settings (AreaSettings)
import Connection (Connection)
import Types (UserId, UserPid, AreaId)
import qualified Area.User as U
import Area.Event (Events)



data Users = Users {connections :: !Connections,
                    connToIds :: !ConnToIds,
                    usersData :: !UsersData,
                    userPids :: !UserPids,
                    userPidToIds :: !UserPidToIds}

type UsersData = M.Map UserId U.User
type Connections = M.Map UserId Connection
type ConnToIds = M.Map Connection UserId
type UserPids = M.Map UserId UserPid
type UserPidToIds = M.Map UserPid UserId

data State = State {areaId :: AreaId,
                    settings :: AreaSettings,
                    tickNumber :: Int,
                    users :: !Users,
                    events :: !Events,
                    eventsForBroadcast :: !Events}

type State' a = S.State State a

tickNumber' :: Lens State Int
tickNumber' = lens tickNumber (\v s -> s{tickNumber=v})

users' :: Lens State Users
users' = lens users (\v s -> s{users=v})

usersData' :: Lens Users UsersData
usersData' = lens usersData (\v us -> us{usersData=v})

connToIds' :: Lens Users ConnToIds
connToIds' = lens connToIds (\v us -> us{connToIds=v})

connections' :: Lens Users Connections
connections' = lens connections (\v us -> us{connections=v})

userPids' :: Lens Users UserPids
userPids' = lens userPids (\v us -> us{userPids=v})


userPidToIds' :: Lens Users UserPidToIds
userPidToIds' = lens userPidToIds (\v us -> us{userPidToIds=v})


events' :: Lens State Events
events' = lens events (\v s -> s{events=v})

eventsForBroadcast' :: Lens State Events
eventsForBroadcast' = lens eventsForBroadcast (\v s -> s{eventsForBroadcast=v})



addUser :: UserId -> Connection -> UserPid -> U.User -> Users -> Users
addUser uid conn userPid user = (usersData' ^%= M.insert uid user)
                              . (connToIds' ^%= M.insert conn uid)
                              . (connections' ^%= M.insert uid conn)
                              . (userPids' ^%= M.insert uid userPid)
                              . (userPidToIds' ^%= M.insert userPid uid)


updateUser :: (U.User -> U.User) -> UserId -> State -> State
updateUser f uid = usersData' . users' ^%= M.adjust f uid


deleteUser :: UserId -> Users -> Users
deleteUser uid us = fun us
    where conn = connections us M.! uid
          pid = userPids us M.! uid
          fun = (usersData' ^%= M.delete uid)
              . (connections' ^%= M.delete uid)
              . (connToIds' ^%= M.delete conn)
              . (userPids' ^%= M.delete uid)
              . (userPidToIds' ^%= M.delete pid)

