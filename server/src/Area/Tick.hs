{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (foldM, liftM, when)
import Control.Category ((>>>))
import Control.Monad.State.Strict (runState, gets, modify)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Data.Lens.Strict (Lens, access, (~=), (+=), (%=))
import Control.Distributed.Process
import Data.Aeson(Value, object, (.=))

import Utils (milliseconds, logInfo, Ts)
import qualified Settings as S
import Area.User (tickClientInfo)
import Area.Utils (broadcastCmd, syncUsers)
import Area.Action (Active(applyActions))
import Area.State
import Area.Event (Event(Appearance, Disappearance), AReason(..), DReason(..))
import Area.Types (Object(..), Destroyable(..), Pos(..), ObjId(UId))
import qualified Area.User as U


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
    when (tnum `rem` logEveryTick == 0) $
        logInfo $ printf "Tick %d of the area '%s'" tnum aid
    when (tnum `rem` syncEveryTick == 0) (syncUsers state)
    return state'

calculateTick :: Ts -> StateS (Maybe Value)
calculateTick ts = do
    handleActions ts
    checkDurability
    handleEvents
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
            evs <- access eventsL
            let (actives', evs') = M.foldrWithKey handle (M.empty, evs) actives
            lens ~= actives'
            eventsL ~= evs'
            return ()
        handle ident active (res, evs) =
            let (active', newEvents) = applyActions ts active
                res' = M.insert ident active' res
            --TODO: use Writer monad
            in (res', newEvents ++ evs)
    handleActive $ usersL >>> usersDataL
    --TODO: update other active objects

checkDurability :: StateS ()
checkDurability =
    M.elems `liftM` access usersL' >>= mapM_ check
    where
        usersL' = usersL >>> usersDataL
        check :: Destroyable b => b -> StateS ()
        check obj =
            when (objDurability obj <= 0) $ do
                eventsL %= (Disappearance (objId obj) Burst :)
                return ()



handleEvents :: StateS ()
handleEvents = do
    evs <- access eventsL
    eventsL ~= []
    evs' <- foldM handle [] evs
    eventsForBroadcastL %= (evs' ++)
    return ()
    where
        handle res event = do
            sendEvent <- handleEvent event
            return $ if sendEvent then event:res else res


handleEvent :: Event -> StateS Bool
handleEvent (Disappearance (UId uid) Burst) = do
    enterPos <- gets $ uncurry Pos . S.enterPos . settings
    let resetUser u = u{U.pos=enterPos, U.durability=1, U.actions=[]}
    modify $ updateUser resetUser uid
    eventsL %= (Appearance uid Recovery :)
    return True
handleEvent _ = return True


tickData :: Ts -> StateS Value
tickData ts = do
    aid <- gets areaId
    evs <- access eventsForBroadcastL
    eventsForBroadcastL ~= []
    usd <- gets $ usersData . users
    let res = object ["areaId" .= aid,
                      "objects" .= usersInfo,
                      "events" .= evs,
                      "timestamp" .= ts]
        usersInfo = map tickClientInfo $ M.elems usd
    return res


