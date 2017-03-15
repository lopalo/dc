{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Base.GlobalCache (
    KeyTag(..), Value(..), NewCacheValue(..),
    globalCacheProcess, isRunning,
    get, getSubscribe, set, durationFactor
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (void, foldM)
import Data.Maybe (isJust)
import Data.List (delete)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)

import Types (Ts, UserName, LogLevel(..))
import qualified Settings as S
import Utils (safeReceive, timeoutForCall, milliseconds)
import Base.GlobalRegistry (Event(NewNode))
import qualified Base.Logger as L


type Record = (Value, Ts)


type KeyRecord = (String, Record)


type KeyValue = (String, Value)


data KeyTag
    = AreaOwnerTag
    | WSAddressTag
    deriving (Eq, Ord, Generic, Typeable)

instance Binary KeyTag


data Value
    = AreaOwner (Maybe UserName)
    | WSAddress String Int
    deriving (Eq, Show, Generic, Typeable)

instance Binary Value


data State = State {
    cache :: M.Map KeyTag (M.Map String Record),
    subscriptions :: M.Map KeyTag (S.Set ProcessId),
    nodes :: [NodeId],
    grEventPort :: ReceivePort Event
    }


data Merge = Merge [KeyRecord] deriving (Generic, Typeable)

instance Binary Merge


data SetValue = SetValue String Value Ts deriving (Generic, Typeable)

instance Binary SetValue


data GetSubscribe =
    GetSubscribe KeyTag (Maybe ProcessId)
    deriving (Generic, Typeable)

instance Binary GetSubscribe


data NewCacheValue = NewCacheValue KeyValue deriving (Generic, Typeable)

instance Binary NewCacheValue


globalCacheServiceName :: String
globalCacheServiceName = "globalCache"


globalCacheProcess :: S.Settings -> ReceivePort Event -> Process ()
globalCacheProcess settings evPort = do
    register globalCacheServiceName =<< getSelfPid
    selfNodeId <- getSelfNode
    let ns = delete selfNodeId $ M.keys $ S.nodeNames $ S.nodes settings
        state = State{
            cache=M.empty,
            subscriptions=M.empty,
            nodes=ns,
            grEventPort=evPort
            }
    void $ loop state


valueTag :: Value -> KeyTag
valueTag (AreaOwner _) = AreaOwnerTag
valueTag (WSAddress _ _) = WSAddressTag


mergeRecord :: State -> KeyRecord -> Process State
mergeRecord state (key, record@(value, _)) = do
    ts <- liftIO milliseconds
    case M.lookup key tagCache of
        Nothing -> broadcast
        Just (value', endTs)
            | value /= value' -> broadcast
            | endTs < ts -> broadcast
            | otherwise -> return ()
    return $ state{cache=M.insert keyTag tagCache' c}
    where
        c = cache state
        keyTag = valueTag value
        tagCache = M.findWithDefault M.empty keyTag c
        tagCache' = M.insert key record tagCache
        tagSubs =
            S.toList $ M.findWithDefault S.empty keyTag $ subscriptions state
        payload = NewCacheValue (key, value)
        broadcast = do
            mapM_ (`send` payload) tagSubs
            log Debug $ "Key updated: " ++ show key


loop :: State -> Process State
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        prepareCall h = callResponse (h state)
        grEventHandler =
            matchChan (grEventPort state) (handleGlobalRegistryEvent state)
        handlers = [
            prepareCall handleGetSubscribe,
            prepare handleSetValue,
            prepare handleMerge,
            prepare handleMonitorNotification,
            grEventHandler,
            matchUnknown (return state)
            ]


handleGetSubscribe :: State -> GetSubscribe -> Process ([KeyValue], State)
handleGetSubscribe state (GetSubscribe keyTag maybePid) = do
    ts <- liftIO milliseconds
    let subs = subscriptions state
        tagCache = M.findWithDefault M.empty keyTag $ cache state
        tagSubs = M.findWithDefault S.empty keyTag subs
        state' =
            case maybePid of
                Nothing -> state
                Just pid ->
                    let subs' = M.insert keyTag (S.insert pid tagSubs) subs
                    in state{subscriptions=subs'}
        kvs = M.toList tagCache
        res = [(key, val) | (key, (val, endTs)) <- kvs, endTs >= ts]
    return (res, state')


handleSetValue :: State -> SetValue -> Process State
handleSetValue state (SetValue key value duration) = do
    ts <- liftIO milliseconds
    let endTs = ts + duration
        keyRecord = (key, (value, endTs))
        merge = Merge [keyRecord]
    mapM_ (\n -> nsendRemote n globalCacheServiceName merge) (nodes state)
    mergeRecord state keyRecord


handleMerge :: State -> Merge -> Process State
handleMerge state (Merge keyRecords) = foldM mergeRecord state keyRecords


handleGlobalRegistryEvent :: State -> Event -> Process State
handleGlobalRegistryEvent state (NewNode nodeId nodeName) = do
    nsendRemote nodeId globalCacheServiceName merge
    log Debug $ "Send merge to node: " ++ nodeName
    return state
    where merge = Merge $ concatMap M.toList $ M.elems $ cache state


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state (ProcessMonitorNotification _ pid _) =
    return state{subscriptions=del $ subscriptions state}
    where del = M.map $ S.delete pid


log :: LogLevel -> String -> Process ()
log level txt = L.log level $ "GlobalCache - " ++ txt


request :: (Serializable a, Serializable b) => a -> TagPool -> Process b
request msg tagPool = do
    Just regPid <- whereis globalCacheServiceName
    tag <- getTag tagPool
    Just res <- callTimeout regPid msg tag timeoutForCall
    return res


--external interface


isRunning :: Process Bool
isRunning = isJust <$> whereis globalCacheServiceName


get :: KeyTag -> TagPool -> Process [KeyValue]
get keyTag = request $ GetSubscribe keyTag Nothing


getSubscribe :: KeyTag -> TagPool -> Process [KeyValue]
getSubscribe keyTag tagPool = do
    pid <- getSelfPid
    request (GetSubscribe keyTag (Just pid)) tagPool


set :: String -> Value -> Ts -> Process ()
set key value duration = nsend globalCacheServiceName payload
    where payload = SetValue key value duration


durationFactor :: Int
durationFactor = 10

