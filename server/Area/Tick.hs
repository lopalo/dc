{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Tick (handleTick, scheduleTick) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (when)
import Control.Monad.State (execStateT, gets, lift)
import Control.Concurrent (threadDelay)
import qualified Data.Map as M
import Text.Printf (printf)

import Data.Lens.Lazy (access, (~=), (+=), (%=))
import Control.Distributed.Process
import Data.Aeson(Value)

import Utils (milliseconds, logDebug)
import qualified Settings as S
import Area.User (tickClientInfo)
import Area.Utils (broadcastCmd')
import Area.Action (Active(applyActions))
import Area.State


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


scheduleTick :: Int -> Process ProcessId
scheduleTick ms = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO $ threadDelay $ ms * 1000
        send selfPid TimeTick


handleTick :: TimeTick -> State -> Process State
handleTick TimeTick = execStateT handleTick' where
    handleTick' :: State' ()
    handleTick' = do
        lift $ scheduleTick S.areaTickMilliseconds
        liftIO milliseconds >>= (timestamp' ~=)
        tnum <- access tickNumber'
        let broadcast = tnum `rem` S.areaBroadcastEveryTick == 0
        handleActions
        when broadcast $ do
            broadcastCmd' "tick" =<< tickClientData
            aid <- gets areaId
            lift $ logDebug $ printf "Broadcast tick %d of area '%s'" tnum aid
        tickNumber' += 1
        return ()

handleActions :: State' ()
handleActions = do
    now <- access timestamp'
    let foldActive :: (Ord a, Active b) => M.Map a b -> M.Map a b
        foldActive = M.foldWithKey handleActive M.empty
        handleActive ident act res =
            case applyActions now act of
                Nothing -> res
                Just act' -> M.insert ident act' res
    users' %= foldActive
    --TODO: update connections and userIds
    --TODO: update other active objects
    return ()

tickClientData :: State' [Value]
tickClientData = do
    us <- gets users
    return $ map tickClientInfo $ M.elems us


