
module Area.State where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad (void)
import Data.Function ((&))
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import Data.Sequence ((|>))

import Data.Lens.Strict (Lens, lens, mapLens, (^%=), (%=))
import Data.Lens.Partial.Common (PartialLens, totalLens, justLens)
import Control.Distributed.Process (Process)

import WS.Connection (Connection)
import Types (UserId, UserName, UserMonitorRef, AreaId, Ts)
import Area.Types (ObjId)
import qualified Area.Objects.User as U
import qualified Area.Objects.Gate as G
import qualified Area.Objects.Asteroid as A
import qualified Area.Objects.ControlPoint as CP
import Area.Logic.Signal (Signal, Signals)
import Area.Logic.Collision (Colliders)
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
    minDBReplicas :: !Int,
    tickNumber :: !Int,
    tickDurations :: ![Ts],
    currentTs :: !Ts,
    users :: !Users,
    gates :: !Gates,
    asteroids :: !Asteroids,
    controlPoints :: !ControlPoints,
    colliders :: !Colliders,
    signalBuffer :: !Signals,
    signalsForBroadcast :: !Signals,
    sideEffects :: ![Process ()]
    }


type StateS a = S.State State a


tickNumberL :: Lens State Int
tickNumberL = lens tickNumber (\v s -> s{tickNumber=v})


tickDurationsL :: Lens State [Ts]
tickDurationsL = lens tickDurations (\v s -> s{tickDurations=v})


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
        else foldl (&) us fs
    where
        fs = [
            userMonitorRefIndexL ^%= M.insert (U.monitorRef user) uid,
            connectionIndexL ^%= M.insert (U.connection user) uid,
            usersDataL ^%= M.insert uid user
            ]
        uid = U.userId user


deleteUser :: UserId -> Users -> Users
deleteUser uid us = foldl (&) us fs
    where
        usr = usersData us M.! uid
        conn = U.connection usr
        ref = U.monitorRef usr
        fs = [
            userMonitorRefIndexL ^%= M.delete ref,
            connectionIndexL ^%= M.delete conn,
            usersDataL ^%= M.delete uid
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


sideEffectsL :: Lens State [Process ()]
sideEffectsL = lens sideEffects (\v s -> s{sideEffects=v})


addSignal :: Signal -> State -> State
addSignal signal = signalsL ^%= (|> signal)


addSignalS :: Signal -> StateS ()
addSignalS signal = void $ signalsL %= (|> signal)


addSideEffectS :: Process () -> StateS ()
addSideEffectS eff = void $ sideEffectsL %= (eff :)


ownerName :: State -> Maybe UserName
ownerName state =
    let cps = controlPoints state
        (_, cp) = M.findMin cps
    in if M.null cps then Nothing else fmap snd $ CP.owner cp
