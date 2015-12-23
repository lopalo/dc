
module Area.State where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad (void)
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M

import Data.Lens.Strict (Lens, lens, mapLens, (^%=), (%=))
import Data.Lens.Partial.Common (PartialLens, totalLens, justLens)

import Connection (Connection)
import Utils (Ts)
import Types (UserId, UserPid, AreaId)
import Area.Types (ObjId)
import qualified Area.Objects.User as U
import qualified Area.Objects.Gate as G
import qualified Area.Objects.Asteroid as A
import Area.Signal (Signal, Signals)
import Area.Collision (Colliders)
import Area.Settings (Settings)


data Users = Users {
    connections :: !Connections,
    connToIds :: !ConnToIds,
    usersData :: !UsersData,
    userPids :: !UserPids,
    userPidToIds :: !UserPidToIds
    }


type UsersData = M.Map UserId U.User


type Connections = M.Map UserId Connection


type ConnToIds = M.Map Connection UserId


type UserPids = M.Map UserId UserPid


type UserPidToIds = M.Map UserPid UserId


type Gates = M.Map ObjId G.Gate


type Asteroids = M.Map ObjId A.Asteroid


data State = State {
    areaId :: !AreaId,
    settings :: !Settings,
    tickNumber :: !Int,
    previousTs :: !Ts,
    users :: !Users,
    gates :: !Gates,
    asteroids :: !Asteroids,
    colliders :: !Colliders,
    signalBuffer :: !Signals,
    signalsForBroadcast :: !Signals
    }


type StateS a = S.State State a


tickNumberL :: Lens State Int
tickNumberL = lens tickNumber (\v s -> s{tickNumber=v})


usersL :: Lens State Users
usersL = lens users (\v s -> s{users=v})


usersDataL :: Lens Users UsersData
usersDataL = lens usersData (\v us -> us{usersData=v})


connToIdsL :: Lens Users ConnToIds
connToIdsL = lens connToIds (\v us -> us{connToIds=v})


connectionsL :: Lens Users Connections
connectionsL = lens connections (\v us -> us{connections=v})


userPidsL :: Lens Users UserPids
userPidsL = lens userPids (\v us -> us{userPids=v})


userPidToIdsL :: Lens Users UserPidToIds
userPidToIdsL = lens userPidToIds (\v us -> us{userPidToIds=v})


insertUser :: UserId -> Connection -> UserPid -> U.User -> Users -> Users
insertUser uid conn userPid user us = foldr ($) us fs
    where
        fs = [
            usersDataL ^%= M.insert uid user,
            connToIdsL ^%= M.insert conn uid,
            connectionsL ^%= M.insert uid conn,
            userPidsL ^%= M.insert uid userPid,
            userPidToIdsL ^%= M.insert userPid uid
            ]


deleteUser :: UserId -> Users -> Users
deleteUser uid us = foldr ($) us fs
    where
        conn = connections us M.! uid
        pid = userPids us M.! uid
        fs = [
            usersDataL ^%= M.delete uid,
            connectionsL ^%= M.delete uid,
            connToIdsL ^%= M.delete conn,
            userPidsL ^%= M.delete uid,
            userPidToIdsL ^%= M.delete pid
            ]


userPL :: UserId -> PartialLens State U.User
userPL = (`objectPL` usersDataL . usersL)


userFieldPL :: UserId -> Lens U.User a -> PartialLens State a
userFieldPL uid = objFieldPL uid (usersDataL . usersL)


asteroidPL :: ObjId -> PartialLens State A.Asteroid
asteroidPL = (`objectPL` asteroidsL)


asteroidFieldPL :: ObjId -> Lens A.Asteroid a -> PartialLens State a
asteroidFieldPL aid = objFieldPL aid asteroidsL


gatesL :: Lens State Gates
gatesL = lens gates (\v s -> s{gates=v})


asteroidsL :: Lens State Asteroids
asteroidsL = lens asteroids (\v s -> s{asteroids=v})


objectPL :: Ord k => k -> Lens State (M.Map k v) -> PartialLens State v
objectPL uid l = justLens . totalLens (mapLens uid . l)


objFieldPL ::
    Ord k => k -> Lens State (M.Map k v) -> Lens v a -> PartialLens State a
objFieldPL uid l l' = totalLens l' . objectPL uid l


collidersL :: Lens State Colliders
collidersL = lens colliders (\v s -> s{colliders=v})


signalsL :: Lens State Signals
signalsL = lens signalBuffer (\v s -> s{signalBuffer=v})

signalsForBroadcastL :: Lens State Signals
signalsForBroadcastL =
    lens signalsForBroadcast (\v s -> s{signalsForBroadcast=v})


addSignal :: Signal -> State -> State
addSignal signal = signalsL ^%= (signal :)


addSignalS :: Signal -> StateS ()
addSignalS signal = void $ signalsL %= (signal :)
