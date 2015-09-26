{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module App.Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding ((.))
import Control.Monad (foldM, liftM, when)
import Control.Category ((>>>), (.))
import Control.Monad.State.Strict (runState, gets, modify)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Data.Lens.Strict (Lens, access, (^$), (~=), (+=), (%=))
import Control.Distributed.Process
import Data.Aeson (ToJSON, Value, object, (.=))

import App.Utils (milliseconds, logInfo, Ts)
import qualified App.Settings as S
import qualified App.Connection as C
import App.Area.User (tickClientInfo)
import App.Area.Action (Active(applyActions))
import App.Area.State
import App.Area.Signal (Signal(Appearance, Disappearance),
                        AReason(..), DReason(..))
import App.Area.Types (Object(..), Destroyable(..), Pos(..), ObjId(UId))
import qualified App.Area.User as U
import qualified App.User.External as UE


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
    scheduleTick $ (S.tickMilliseconds . settings) state
    now <- liftIO milliseconds
    let tnum = tickNumber state
        (broadcastData, state') = runState (calculateTick now) state
        aid = areaId state'
    case broadcastData of
        Just bd -> broadcastCmd state "tick" bd
        Nothing -> return ()
    let logEveryTick = (S.logEveryTick . settings) state
        syncEveryTick = (S.syncEveryTick . settings) state
    --TODO: timers that are processed only inside ticks. Save the last time
    --of each timer in the state
    when (tnum `rem` logEveryTick == 0) $
        logInfo $ printf "Tick %d of the area '%s'" tnum aid
    when (tnum `rem` syncEveryTick == 0) (syncUsers state)
    return state'

calculateTick :: Ts -> StateS (Maybe Value)
calculateTick ts = do
    handleActions ts
    checkDurability
    handleSignals
    tnum <- access tickNumberL
    tickNumberL += 1
    broadcastEveryTick <- gets $ S.broadcastEveryTick . settings
    let broadcast = tnum `rem` broadcastEveryTick == 0
    if broadcast
        then liftM Just (tickData ts)
        else return Nothing

handleActions :: Ts -> StateS ()
handleActions ts = do
    let handleActive :: (Ord a, Active b) => Lens State (M.Map a b) -> StateS ()
        handleActive lens = do
            actives <- access lens
            signals <- access signalsL
            let (actives', signals') = M.foldrWithKey handle
                                                      (M.empty, signals)
                                                      actives
            lens ~= actives'
            signalsL ~= signals'
            return ()
        handle ident active (res, signals) =
            let (active', newSignals) = applyActions ts active
                res' = M.insert ident active' res
            --TODO: use Writer monad with Set as Monoid;
            --      use foldM, toAscList, fromAscList
            in (res', newSignals ++ signals)
    handleActive $ usersL >>> usersDataL
    --TODO: update other active objects

checkDurability :: StateS ()
checkDurability =
    M.elems `liftM` access usersL' >>= mapM_ check
    where
        usersL' = usersL >>> usersDataL
        check :: Destroyable b => b -> StateS ()
        check obj =
            when (objDurability obj <= 0) $
                addSignalS $ Disappearance (objId obj) Burst



handleSignals :: StateS ()
handleSignals = do
    signals <- access signalsL
    signalsL ~= []
    signals' <- foldM handle [] signals
    signalsForBroadcastL %= (signals' ++)
    return ()
    where
        handle res signal = do
            sendSignal <- handleSignal signal
            return $ if sendSignal then signal : res else res


handleSignal :: Signal -> StateS Bool
handleSignal (Disappearance (UId uid) Burst) = do
    enterPos <- gets $ uncurry Pos . S.enterPos . settings
    let resetUser u = u{U.pos=enterPos, U.durability=1, U.actions=[]}
    modify $ updateUser resetUser uid
    addSignalS $ Appearance uid Recovery
    return True
handleSignal _ = return True


tickData :: Ts -> StateS Value
tickData ts = do
    aid <- gets areaId
    signals <- access signalsForBroadcastL
    signalsForBroadcastL ~= []
    usd <- gets $ usersData . users
    let res = object ["areaId" .= aid,
                      "objects" .= usersInfo,
                      "signals" .= signals,
                      "timestamp" .= ts]
        usersInfo = map tickClientInfo $ M.elems usd
    return res


syncUsers :: State -> Process ()
syncUsers state = mapM_ sync us
    where
        sync usr = let pid = uPids M.! U.userId usr
                   in UE.syncState pid $ U.userArea usr aid
        aid = areaId state
        uPids = userPidsL . usersL ^$ state
        us = M.elems $ usersDataL . usersL ^$ state


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd = C.broadcastCmd (M.elems cs) ("area." ++ cmd)
    where cs = connectionsL . usersL ^$ state

