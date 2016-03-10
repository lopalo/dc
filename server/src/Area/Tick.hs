{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log, (.))
import Control.Applicative ((<$>))
import Control.Monad (liftM, when, unless)
import Control.Monad.Writer (runWriter)
import Control.Category ((>>>), (.))
import Control.Monad.State.Strict (runState, get, gets, modify)
import Data.Foldable (foldlM, toList)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Text.Printf (printf)

import Data.Lens.Strict (access, (^$), (~=), (+=), (%=))
import Data.Lens.Partial.Common (getPL, (^%=), (^-=), (^+=))
import Control.Distributed.Process
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)
import Data.Aeson (ToJSON, Value, object, (.=))

import Types (Ts, LogLevel(..))
import Utils (milliseconds)
import Base.Logger (log)
import qualified DB.DB as DB
import qualified WS.Connection as C
import qualified Area.Settings as AS
import Area.Action (
    Active(applyActions), Time(..),
    Action(
        MoveDistance, MoveCircularTrajectory,
        startTs, endTs, startPos, endPos
        ),
    actionsL
    )
import Area.State
import Area.Collision (
    Collision, emptyColliders, addCollider,
    findCollisions, collisionPair
    )
import Area.Signal (
    Signal(Appearance, Disappearance, MoveAsteroid),
    AReason(..), DReason(..)
    )
import Area.Utils (distance)
import Area.Misc (spawnUser)
import Area.Types (Object(..), Destroyable(..), ObjId(..))
import qualified Area.Objects.User as U
import qualified Area.Objects.ControlPoint as CP
import qualified User.External as UE


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


scheduleTick :: Ts -> Process ProcessId
scheduleTick ms = do
    selfPid <- getSelfPid
    spawnLocal $ sleepFor ms Millis >> send selfPid TimeTick


handleTick :: State -> TimeTick -> Process State
handleTick state TimeTick = do
    scheduleTick $ (AS.tickMilliseconds . settings) state
    now <- liftIO milliseconds
    let tnum = tickNumber state
        (broadcastData, state') = runState (calculateTick now) state
        aid = areaId state'
    case broadcastData of
        Just bd -> broadcastCmd state "tick" bd
        Nothing -> return ()
    let logEveryTick = (AS.logEveryTick . settings) state
        syncEveryTick = (AS.syncEveryTick . settings) state
    when (tnum `rem` logEveryTick == 0) $
        log Info $ printf "Tick %d of the '%s'" tnum $ show aid
    when (tnum `rem` syncEveryTick == 0) $ do
        syncUsers state
        saveObjects state
    return state'


calculateTick :: Ts -> StateS (Maybe Value)
calculateTick ts = do
    previousTs <- gets currentTs
    modify $ \s -> s{currentTs=ts}
    handleActions previousTs
    handleCollisions
    checkDurability
    handleSignals
    tnum <- access tickNumberL
    tickNumberL += 1
    broadcastEveryTick <- gets $ AS.broadcastEveryTick . settings
    let broadcast = tnum `rem` broadcastEveryTick == 0
    if broadcast
        then liftM Just (tickData ts)
        else return Nothing


handleActions :: Ts -> StateS ()
handleActions previousTs= do
    ts <- gets currentTs
    let
        handleActive lens = do
            actives <- access lens
            signals <- access signalsL
            let
                (actives', signals') =
                    M.foldlWithKey handle (M.empty, signals) actives
            lens ~= actives'
            signalsL ~= signals'
            return ()
        handle (res, signals) ident active =
            let (active', newSignals) = runWriter $ applyActions time active
                res' = M.insert ident active' res
            in (res', signals Seq.>< newSignals)
        time = Time{timestamp=ts, timeDelta=ts - previousTs}
    handleActive $ usersL >>> usersDataL
    handleActive gatesL
    handleActive asteroidsL
    handleActive controlPointsL


handleCollisions :: StateS ()
handleCollisions = do
    collidersL ~= emptyColliders
    addColliders $ usersL >>> usersDataL
    addColliders asteroidsL
    addColliders controlPointsL
    colliders_ <- access collidersL
    let
        find collisions collider =
            let collisions' = findCollisions collider colliders_
            in Set.union collisions collisions'
        collisions'' = Set.foldl find Set.empty colliders_
    mapM_ handleCollision $ Set.toAscList collisions''
    return ()
    where
        addColliders lens = M.elems <$> access lens >>= mapM_ addColl
        addColl obj = collidersL %= addCollider obj


handleCollision :: Collision -> StateS()
handleCollision collision =
    case collisionPair collision of
        (UId uid, aid@AsteroidId{}) ->
            decreaseDurability (userFieldPL uid) (asteroidFieldPL aid)
        (aid@AsteroidId{}, aid'@AsteroidId{}) ->
            decreaseDurability (asteroidFieldPL aid) (asteroidFieldPL aid')
        (UId uid, UId uid') ->
            decreaseDurability (userFieldPL uid) (userFieldPL uid')
        (UId uid, cpid@CPId{}) ->
            decreaseDurability (userFieldPL uid) (cpFieldPL cpid)
        _ -> return ()
    where
        decreaseDurability l l' = do
            let durLens = l durabilityL
                durLens' = l' durabilityL
            mDurabiility <- gets $ getPL durLens
            mDurabiility' <- gets $ getPL durLens'
            case (mDurabiility, mDurabiility') of
                (Just dur, Just dur') -> do
                    let dur'' = min dur dur'
                    modify $ durLens ^-= dur''
                    modify $ durLens' ^-= dur''
                _ -> return ()


checkDurability :: StateS ()
checkDurability = do
    checkObjects $ usersL >>> usersDataL
    checkObjects asteroidsL
    checkObjects controlPointsL
    where
        checkObjects lens = M.elems <$> access lens >>= mapM_ check
        check obj =
            when (getDurability obj <= 0) $
                addSignalS $ Disappearance (objId obj) Destruction


handleSignals :: StateS ()
handleSignals = do
    signals <- access signalsL
    signalsL ~= Seq.empty
    signals' <- foldlM handle Seq.empty signals
    signalsForBroadcastL %= (Seq.>< signals')
    return ()
    where
        handle res signal = do
            maybeSendSignal <- handleSignal signal
            return $
                case maybeSendSignal of
                    Nothing -> res
                    Just signal' -> res Seq.|> signal'


handleSignal :: Signal -> StateS (Maybe Signal)
handleSignal signal@(Disappearance (UId uid) Destruction) = do
    mAttacker <- gets $  getPL $ userFieldPL uid U.lastAttackerL
    let
        resetUser u = u{
            U.durability=1,
            U.actions=[],
            U.deaths=U.deaths u + 1,
            U.lastAttacker=Nothing
            }
    modify $ userPL uid ^%= resetUser
    modify $ spawnUser uid
    addSignalS $ Appearance uid Recovery
    case mAttacker of
        Just (Just attackerId) ->
            modify $ userFieldPL attackerId U.killsL ^+= 1
        _ -> return ()
    return $ Just signal
handleSignal signal@(Disappearance aid@(AsteroidId _) Destruction) = do
    asteroidsL %= M.delete aid
    return $ Just signal
handleSignal (Disappearance cpid@(CPId _) Destruction) = do
    modify $ cpPL cpid ^%= \cp -> cp{CP.durability=1, CP.owner=Nothing}
    return Nothing
handleSignal (MoveAsteroid aid targetPos) = do
    ts <- gets currentTs
    speed <- gets $ AS.asteroidPullSpeed . settings
    mPos <- gets $ getPL $ asteroidFieldPL aid posL
    let asLens = asteroidFieldPL aid actionsL

        moveTrajectory MoveCircularTrajectory{} = True
        moveTrajectory _ = False

        moveDistance MoveDistance{} = True
        moveDistance _ = False

        astPos = fromJust mPos
        action = MoveDistance{
            startTs=ts,
            endTs=ts + round dt,
            startPos=astPos,
            endPos=targetPos
            }
        dt = distance astPos targetPos / (speed / 1000)
    modify $ asLens ^%= filter (not . moveDistance)
    mActions <- gets $ getPL asLens
    case mActions of
        Just as ->
            unless (any moveTrajectory as) $
                modify $ asLens ^%= (action :)
        Nothing -> return ()
    return Nothing
handleSignal signal = return $ Just signal


tickData :: Ts -> StateS Value
tickData ts = do
    aid <- gets areaId
    signals <- access signalsForBroadcastL
    signalsForBroadcastL ~= Seq.empty
    state <- get
    let
        res =
            object [
                "areaId" .= aid,
                "objects" .= concat objects,
                "signals" .= toList signals,
                "timestamp" .= ts
                ]
        g getter = map tickClientInfo $ M.elems $ getter state
        objects = [
            g (usersData . users),
            g gates,
            g asteroids,
            g controlPoints
            ]
    return res


syncUsers :: State -> Process ()
syncUsers state = mapM_ sync us
    where
        sync usr = UE.syncState (U.pid usr) (U.userArea usr)
        us = M.elems $ usersDataL . usersL ^$ state


saveObjects :: State -> Process ()
saveObjects state = DB.putAreaObjects (areaId state) objs
    where
        g getter = M.elems $ getter state
        objs = DB.AreaObjects (g gates) (g asteroids) (g controlPoints)


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd = C.broadcastCmd (M.keys cs) ("area." ++ cmd)
    where
        cs = connectionIndex . users $ state

