{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (when, foldM)
import Control.Monad.State.Strict (execStateT, gets, lift)
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

import Data.Lens.Common (Lens)
import Data.Lens.Strict (access, (~=), (+=), (%=))
import Control.Distributed.Process
import Data.Aeson(Value, object, (.=))

import Utils (milliseconds, logDebug)
import Types (UserId(UserId))
import qualified Settings as S
import Area.User (tickClientInfo)
import Area.Utils (broadcastCmd')
import Area.Action (Active(applyActions))
import Area.State
import Area.Event (Event(DeleteUser))


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


scheduleTick :: Int -> Process ProcessId
scheduleTick ms = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO $ threadDelay $ ms * 1000
        send selfPid TimeTick


--TODO: maybe use State instead of StateT to make the function is pure
handleTick :: TimeTick -> State -> Process State
handleTick TimeTick = execStateT handleTick' where
    handleTick' :: State' ()
    handleTick' = do
        lift $ scheduleTick S.areaTickMilliseconds
        liftIO milliseconds >>= (timestamp' ~=)
        tnum <- access tickNumber'
        let broadcast = tnum `rem` S.areaBroadcastEveryTick == 0
        handleActions
        handleEvents
        when broadcast $ do
            broadcastCmd' "tick" =<< tickData
            aid <- gets areaId
            lift $ logDebug $ printf "Broadcast tick %d of area '%s'" tnum aid
        tickNumber' += 1
        return ()

handleActions :: State' ()
handleActions = do
    now <- access timestamp'
    let handleActive :: (Ord a, Active b) => Lens State (M.Map a b) -> State' ()
        handleActive lens = do
            as <- access lens
            evs <- access events'
            let (as', evs') = M.foldrWithKey handle (M.empty, evs) as
            lens ~= as'
            events' ~= evs'
            return ()
        handle ident act (res, evs) =
            let (act', newEvents) = applyActions now act
                res' = M.insert ident act' res
            in (res', newEvents ++ evs)
    handleActive users'
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
handleEvent (DeleteUser userId) = do
    users' %= M.delete userId
    conns <- access connections'
    connections' %= M.delete userId
    userIds' %= M.delete (conns M.! userId)
    --TODO remove areaId from user's connection or disconnect him
    return True


tickData :: State' Value
tickData = do
    evs <- access eventsForBroadcast'
    eventsForBroadcast' ~= []
    us <- gets users
    let res = object ["objects" .= usersInfo,
                      "events" .= evs]
        usersInfo = map tickClientInfo $ M.elems us
    return res


