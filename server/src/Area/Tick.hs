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

import Data.Lens.Common (Lens)
import Data.Lens.Strict (access, (~=), (+=), (%=))
import Control.Distributed.Process
import Data.Aeson(Value, object, (.=))

import Utils (milliseconds, logInfo, Ts)
import qualified Settings as S
import Area.User (tickClientInfo)
import Area.Utils (broadcastCmd, syncUsers)
import Area.Action (Active(applyActions))
import Area.State
import Area.Event
import Area.Types (Pos(..))
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

calculateTick :: Ts -> State' (Maybe Value)
calculateTick ts = do
    handleActions ts
    handleEvents
    tnum <- access tickNumber'
    tickNumber' += 1
    broadcastEveryTick <- gets $ S.broadcastEveryTick . settings
    let broadcast = tnum `rem` broadcastEveryTick == 0
    if broadcast
        then liftM Just (tickData ts)
        else return Nothing

handleActions :: Ts -> State' ()
handleActions ts = do
    let handleActive :: (Ord a, Active b) => Lens State (M.Map a b) -> State' ()
        handleActive lens = do
            as <- access lens
            evs <- access events'
            let (as', evs') = M.foldrWithKey handle (M.empty, evs) as
            lens ~= as'
            events' ~= evs'
            return ()
        handle ident act (res, evs) =
            let (act', newEvents) = applyActions ts act
                res' = M.insert ident act' res
            in (res', newEvents ++ evs)
    handleActive $ users' >>> usersData'
    --TODO: update other active objects
    return ()

handleEvents :: State' ()
handleEvents = do
    evs <- access events'
    events' ~= []
    evs' <- foldM handle [] evs
    eventsForBroadcast' %= (evs' ++)
    return ()
    where
        handle res event = do
            sendEvent <- handleEvent event
            return $ if sendEvent then event:res else res


handleEvent :: Event -> State' Bool
handleEvent (Disappearance uid Burst) = do
    enterPos <- gets $ uncurry Pos . S.enterPos . settings
    let resetUser u = u{U.pos=enterPos, U.durability=1, U.actions=[]}
    modify $ updateUser resetUser uid
    events' %= (Appearance uid Recovery :)
    return True
handleEvent _ = return True


tickData :: Ts -> State' Value
tickData ts = do
    aid <- gets areaId
    evs <- access eventsForBroadcast'
    eventsForBroadcast' ~= []
    usd <- gets $ usersData . users
    let res = object ["areaId" .= aid,
                      "objects" .= usersInfo,
                      "events" .= evs,
                      "timestamp" .= ts]
        usersInfo = map tickClientInfo $ M.elems usd
    return res


