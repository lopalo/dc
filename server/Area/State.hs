
module Area.State where

import Control.Monad.State.Strict (StateT)
import qualified Data.Map.Strict as M

import Data.Lens.Common (lens, Lens)
import Control.Distributed.Process

import Connection (Connection)
import Types (UserId, AreaId)
import qualified Area.User as U
import Area.Types (Ts)
import Area.Event (Events)




type Connections = M.Map UserId Connection
type Users = M.Map UserId U.User
type UserIds = M.Map Connection UserId

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    timestamp :: Ts,
                    connections :: !Connections,
                    userIds :: !UserIds,
                    users :: !Users,
                    events :: !Events,
                    eventsForBroadcast :: !Events} --TODO: add settings

type State' a = StateT State Process a

tickNumber' :: Lens State Int
tickNumber' = lens tickNumber (\v s -> s{tickNumber=v})

timestamp' :: Lens State Ts
timestamp' = lens timestamp (\v s -> s{timestamp=v})

users' :: Lens State Users
users' = lens users (\v s -> s{users=v})

userIds' :: Lens State UserIds
userIds' = lens userIds (\v s -> s{userIds=v})

connections' :: Lens State Connections
connections' = lens connections (\v s -> s{connections=v})

events' :: Lens State Events
events' = lens events (\v s -> s{events=v})

eventsForBroadcast' :: Lens State Events
eventsForBroadcast' = lens eventsForBroadcast (\v s -> s{eventsForBroadcast=v})



