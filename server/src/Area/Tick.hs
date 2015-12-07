{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding ((.))
import Control.Monad (foldM, liftM, when, unless, void)
import Control.Category ((>>>), (.))
import Control.Monad.State.Strict (runState, gets, modify)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Text.Printf (printf)

import Data.Lens.Strict (access, (^$), (~=), (+=), (%=), (^+=), (^-=))
import Control.Distributed.Process
import Data.Aeson (ToJSON, Value, object, (.=))

import Utils (milliseconds, logInfo, Ts)
import DB (putAreaObjects)
import qualified Connection as C
import qualified Area.Settings as AS
import Area.Action (Active(applyActions), Time(..))
import Area.State
import Area.Collision (Collision, emptyColliders, addCollider,
                       findCollisions, collisionPair)
import Area.Signal (Signal(Appearance, Disappearance),
                    AReason(..), DReason(..))
import Area.Types (Object(..), Destroyable(..), Pos(..), ObjId(..))
import qualified Area.Objects.User as U
import qualified User.External as UE


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


scheduleTick :: Ts -> Process ProcessId
scheduleTick ms = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO $ threadDelay $ ms * 1000
        send selfPid TimeTick


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
    --TODO: timers that are processed only inside ticks. Save the last time
    --of each timer in the state
    when (tnum `rem` logEveryTick == 0) $
        logInfo $ printf "Tick %d of the '%s'" tnum $ show aid
    when (tnum `rem` syncEveryTick == 0) $ do
        syncUsers state
        saveObjects state
    return state'

calculateTick :: Ts -> StateS (Maybe Value)
calculateTick ts = do
    handleActions ts
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
handleActions ts = do
    pts <- gets previousTs
    modify $ \s -> s{previousTs=ts}
    let handleActive lens = do
            actives <- access lens
            signals <- access signalsL
            let (actives', signals') = M.foldrWithKey handle
                                                      (M.empty, signals)
                                                      actives
            lens ~= actives'
            signalsL ~= signals'
            return ()
        handle ident active (res, signals) =
            let (active', newSignals) = applyActions time active
                res' = M.insert ident active' res
            --TODO: use Writer monad with Set as Monoid;
            --      use foldM, toAscList, fromAscList
            in (res', newSignals ++ signals)
        time = Time{timestamp=ts, timeDelta=ts - pts}
    handleActive $ usersL >>> usersDataL
    handleActive gatesL
    handleActive asteroidsL


handleCollisions :: StateS ()
handleCollisions = do
    collidersL ~= emptyColliders
    addColliders $ usersL >>> usersDataL
    addColliders asteroidsL
    colliders <- access collidersL
    let find collisions collider =
            let collisions' = findCollisions collider colliders
            in Set.union collisions collisions'
        collisions = Set.foldl find Set.empty colliders
    mapM_ handleCollision $ Set.toAscList collisions
    return ()
    where
        addColliders lens = M.elems `liftM` access lens >>= mapM_ addColl
        addColl obj = collidersL %= addCollider obj

handleCollision :: Collision -> StateS()
handleCollision collision =
    case collisionPair collision of
        (UId uid, aid@AsteroidId{}) ->
            decreaseDurability (userField uid) (modifyUser uid)
                               (asteroidField aid) (modifyAsteroid aid)
        (aid@AsteroidId{}, aid'@AsteroidId{}) ->
            decreaseDurability (asteroidField aid) (modifyAsteroid aid)
                               (asteroidField aid') (modifyAsteroid aid')
        (UId uid, UId uid') ->
            decreaseDurability (userField uid) (modifyUser uid)
                               (userField uid') (modifyUser uid')
        _ -> return ()
    where
        decreaseDurability getObj modObj getObj' modObj' = do
            mDurabiility <- gets $ getObj getDurability
            mDurabiility' <- gets $ getObj' getDurability
            case (mDurabiility, mDurabiility') of
                (Just dur, Just dur') -> do
                    let dur'' = min dur dur'
                    modify $ modObj $ durabilityL ^-= dur''
                    modify $ modObj' $ durabilityL ^-= dur''
                _ -> return ()


checkDurability :: StateS ()
checkDurability = do
    checkObjects $ usersL >>> usersDataL
    checkObjects asteroidsL
    where
        checkObjects lens = M.elems `liftM` access lens >>= mapM_ check
        check obj =
            when (getDurability obj <= 0) $
                addSignalS $ Disappearance (objId obj) Destruction



handleSignals :: StateS ()
handleSignals = do
    signals <- access signalsL
    signalsL ~= []
    signals' <- foldM handle [] signals
    signalsForBroadcastL %= (signals' ++)
    return ()
    where
        handle res signal = do
            maybeSendSignal <- handleSignal signal
            return $ case maybeSendSignal of
                Nothing -> res
                Just signal' -> signal' : res


handleSignal :: Signal -> StateS (Maybe Signal)
handleSignal signal@(Disappearance (UId uid) Destruction) = do
    enterPos <- gets $ uncurry Pos . AS.enterPos . settings
    mAttacker <- gets $ userField uid U.lastAttacker
    let resetUser u = u{U.pos=enterPos,
                        U.durability=1,
                        U.actions=[],
                        U.deaths=U.deaths u + 1,
                        U.lastAttacker=Nothing}
    modify $ modifyUser uid resetUser
    addSignalS $ Appearance uid Recovery
    case mAttacker of
        Just (Just attackerId) ->
            modify $ modifyUser attackerId $ U.killsL ^+= 1
        _ -> return ()
    return $ Just signal
handleSignal signal@(Disappearance aid@(AsteroidId _) Destruction) = do
    asteroidsL %= M.delete aid
    --TODO: delete from DB
    return $ Just signal
handleSignal signal = return $ Just signal


tickData :: Ts -> StateS Value
tickData ts = do
    aid <- gets areaId
    signals <- access signalsForBroadcastL
    signalsForBroadcastL ~= []
    usd <- gets $ usersData . users
    gs <- gets gates
    as <- gets asteroids
    let res = object ["areaId" .= aid,
                      "objects" .= objects,
                      "signals" .= signals,
                      "timestamp" .= ts]
        usersInfo = map tickClientInfo $ M.elems usd
        gatesInfo = map tickClientInfo $ M.elems gs
        asteroidsInfo = map tickClientInfo $ M.elems as
        objects = usersInfo ++ gatesInfo ++ asteroidsInfo
    return res


syncUsers :: State -> Process ()
syncUsers state = mapM_ sync us
    where
        sync usr = let pid = uPids M.! U.userId usr
                   in UE.syncState pid $ U.userArea usr
        uPids = userPidsL . usersL ^$ state
        us = M.elems $ usersDataL . usersL ^$ state


saveObjects :: State -> Process ()
saveObjects state =
    putAreaObjects (areaId state) objs
    where
        objs = (M.elems $ gates state, M.elems $ asteroids state)


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd = C.broadcastCmd (M.elems cs) ("area." ++ cmd)
    where cs = connectionsL . usersL ^$ state

