
module Area.State where

import Control.Monad.State (StateT)
import qualified Data.Map as M

import Data.Lens.Common (lens, Lens)
import Control.Distributed.Process

import Connection (Connection)
import Types (UserId, AreaId)
import qualified Area.User as U
import Area.Types (Ts)




type Connections = M.Map UserId Connection
type Users = M.Map UserId U.User
type UserIds = M.Map Connection UserId

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    timestamp :: Ts,
                    connections :: Connections,
                    userIds :: UserIds,
                    users :: Users} --TODO: add settings

type State' a = StateT State Process a

tickNumber' :: Lens State Int
tickNumber' = lens tickNumber (\v s -> s{tickNumber=v})

timestamp' :: Lens State Ts
timestamp' = lens timestamp (\v s -> s{timestamp=v})

users' :: Lens State Users
users' = lens users (\v s -> s{users=v})

