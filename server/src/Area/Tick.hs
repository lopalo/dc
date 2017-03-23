{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log, (.))
import Control.Monad (when)
import Control.Category ((.))
import Control.Monad.State.Strict (runState)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Data.Lens.Strict ((^$))
import Control.Distributed.Process
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)

import Types (Ts, LogLevel(..))
import Utils (milliseconds)
import qualified DB.AreaDB as DB
import qualified WS.Connection as C
import qualified Area.Settings as AS
import Area.State
import Area.Utils (log)
import Area.Misc (updateOwnerName)
import qualified Area.Objects.User as U
import qualified Area.Logic.Tick as L
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
        (broadcastData, state') = runState (L.calculateTick now) state
        aid = areaId state'
    state'' <- handleSideEffects state'
    case broadcastData of
        Just bd -> broadcastState bd
        Nothing -> return ()
    let logEveryTick = (AS.logEveryTick . settings) state
        syncEveryTick = (AS.syncEveryTick . settings) state
    when (tnum `rem` logEveryTick == 0) $
        log Info $ printf "Tick %d of the '%s'" tnum $ show aid
    when (tnum `rem` syncEveryTick == 0) $ do
        syncUsers state''
        saveObjects state''
        updateOwnerName state''
    when (ownerName state /= ownerName state'') (updateOwnerName state'')
    return state''


broadcastState :: L.BroadcastData -> Process ()
broadcastState bd = do
    C.broadcastBeginMultipart connections cmd
    mapM_ sendPart bd
    C.broadcastEndMultipart connections cmd
    where
        connections = snd $ head bd
        cmd = "area.tick"
        sendPart (payload, connections') =
            C.broadcastCmd connections' cmd payload


syncUsers :: State -> Process ()
syncUsers state = mapM_ sync us
    where
        sync usr = UE.syncState (U.pid usr) (U.userArea usr)
        us = M.elems $ usersDataL . usersL ^$ state


saveObjects :: State -> Process ()
saveObjects state = DB.putAreaObjects (areaId state) minReplicas objs
    where
        g getter = M.elems $ getter state
        objs = DB.AreaObjects (g gates) (g asteroids) (g controlPoints)
        minReplicas = minDBReplicas state


handleSideEffects :: State -> Process State
handleSideEffects state = do
    sequence_ $ sideEffects state
    return state{sideEffects=[]}


