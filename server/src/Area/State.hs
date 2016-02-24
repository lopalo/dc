
module Area.State where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad (void)
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import Data.Sequence ((|>))

import Data.Lens.Strict (Lens, lens, mapLens, (^%=), (%=))
import Data.Lens.Partial.Common (PartialLens, totalLens, justLens)

import WS.Connection (Connection)
import Types (UserId, UserMonitorRef, AreaId, Ts)
import Area.Types (ObjId)
import qualified Area.Objects.User as U
import qualified Area.Objects.Gate as G
import qualified Area.Objects.Asteroid as A
import qualified Area.Objects.ControlPoint as CP
import Area.Signal (Signal, Signals)
import Area.Collision (Colliders)
import Area.Settings (Settings)


data Users = Users {
    usersData :: !UsersData,
    connectionIndex :: !ConnectionIndex,
    userMonitorRefIndex :: !UserMonitorRefIndex
    }


type UsersData = M.Map UserId U.User


type ConnectionIndex = M.Map Connection UserId


type UserMonitorRefIndex = M.Map UserMonitorRef UserId


type Gates = M.Map ObjId G.Gate


type Asteroids = M.Map ObjId A.Asteroid


type ControlPoints = M.Map ObjId CP.ControlPoint


data State = State {
    areaId :: !AreaId,
    settings :: !Settings,
    tickNumber :: !Int,
    currentTs :: !Ts,
    users :: !Users,
    gates :: !Gates,
    asteroids :: !Asteroids,
    controlPoints :: !ControlPoints,
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


connectionIndexL :: Lens Users ConnectionIndex
connectionIndexL = lens connectionIndex (\v us -> us{connectionIndex=v})


userMonitorRefIndexL :: Lens Users UserMonitorRefIndex
userMonitorRefIndexL =
    lens userMonitorRefIndex (\v us -> us{userMonitorRefIndex=v})


insertUser :: U.User -> Users -> Users
insertUser user us =
    if uid `M.member` usersData us
        then us
        else foldr ($) us fs
    where
        fs = [
            usersDataL ^%= M.insert uid user,
            connectionIndexL ^%= M.insert (U.connection user) uid,
            userMonitorRefIndexL ^%= M.insert (U.monitorRef user) uid
            ]
        uid = U.userId user


deleteUser :: UserId -> Users -> Users
deleteUser uid us = foldr ($) us fs
    where
        usr = usersData  us M.! uid
        conn = U.connection usr
        ref = U.monitorRef usr
        fs = [
            usersDataL ^%= M.delete uid,
            connectionIndexL ^%= M.delete conn,
            userMonitorRefIndexL ^%= M.delete ref
            ]


userPL :: UserId -> PartialLens State U.User
userPL = (`objectPL` usersDataL . usersL)


userFieldPL :: UserId -> Lens U.User a -> PartialLens State a
userFieldPL uid = objFieldPL uid (usersDataL . usersL)


asteroidPL :: ObjId -> PartialLens State A.Asteroid
asteroidPL = (`objectPL` asteroidsL)


asteroidFieldPL :: ObjId -> Lens A.Asteroid a -> PartialLens State a
asteroidFieldPL aid = objFieldPL aid asteroidsL


cpPL :: ObjId -> PartialLens State CP.ControlPoint
cpPL = (`objectPL` controlPointsL)


cpFieldPL :: ObjId -> Lens CP.ControlPoint a -> PartialLens State a
cpFieldPL aid = objFieldPL aid controlPointsL


gatesL :: Lens State Gates
gatesL = lens gates (\v s -> s{gates=v})


asteroidsL :: Lens State Asteroids
asteroidsL = lens asteroids (\v s -> s{asteroids=v})


controlPointsL :: Lens State ControlPoints
controlPointsL = lens controlPoints (\v s -> s{controlPoints=v})


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
addSignal signal = signalsL ^%= (|> signal)


addSignalS :: Signal -> StateS ()
addSignalS signal = void $ signalsL %= (|> signal)
