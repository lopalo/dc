{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Base.GlobalRegistry (
    RegistrationFailure(RegistrationFailure),
    Event(..),
    spawnGlobalRegistry, isRunning,
    getNameList, getVisibleNodes, globalRegister, globalRegisterAsync,
    globalWhereIs, globalMultiWhereIs, globalWhereIsByPrefix, globalNSend,
    multicallByPrefix
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (void, when, forever, foldM)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Maybe (isJust)
import Data.List (delete)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Trie as T

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, newTagPool, getTag)
import Control.Distributed.Process.Extras.Call (
    callResponse, callTimeout, multicall
    )
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)


import Types (NodeName, NodeNames, Ts, LogLevel(..))
import qualified Settings as S
import Utils (safeReceive, timeoutForCall, milliseconds)
import qualified Base.Logger as L


type Record = (Ts, ProcessId)


type PidIndex =  M.Map ProcessId [B.ByteString]


type NameList = [(String, ProcessId, Ts)]


data State = State {
    registry :: T.Trie Record,
    pidIndex :: PidIndex,
    visibleNodes :: M.Map NodeId (Maybe ProcessId),
    nodeNames :: NodeNames,
    eventPort :: SendPort Event
    }


data Ping = Ping NodeId ProcessId deriving (Generic, Typeable)

instance Binary Ping


data Merge = Merge (T.Trie Record) deriving (Generic, Typeable)

instance Binary Merge


data MergeRequest = MergeRequest (T.Trie Record) deriving (Generic, Typeable)

instance Binary MergeRequest


data Register = Register String ProcessId Bool deriving (Generic, Typeable)

instance Binary Register


data RegistrationResult
    = Success
    | Failure
    | Delegate [ProcessId] MergeRequest
    deriving (Generic, Typeable)

instance Binary RegistrationResult


data WhereIs = WhereIs [String] deriving (Generic, Typeable)

instance Binary WhereIs


data WhereIsByPrefix = WhereIsByPrefix String deriving (Generic, Typeable)

instance Binary WhereIsByPrefix


data GetNameList = GetNameList String deriving (Generic, Typeable)

instance Binary GetNameList


data GetVisibleNodes = GetVisibleNodes deriving (Generic, Typeable)

instance Binary GetVisibleNodes


data RegistrationFailure =
    RegistrationFailure
    deriving (Generic, Typeable, Show)

instance Binary RegistrationFailure


data Event = NewNode NodeId NodeName deriving (Generic, Typeable)

instance Binary Event


globalRegistryServiceName :: String
globalRegistryServiceName = "globalRegistry"


spawnGlobalRegistry :: S.Settings -> Process (ReceivePort Event)
spawnGlobalRegistry settings = do
    mvar <- liftIO newEmptyMVar
    spawnLocal $ do
        (sendPort, receivePort) <- newChan
        liftIO $ putMVar mvar receivePort
        globalRegistryProcess settings sendPort
    liftIO $ takeMVar mvar


globalRegistryProcess :: S.Settings -> SendPort Event -> Process ()
globalRegistryProcess settings evPort = do
    register globalRegistryServiceName =<< getSelfPid
    selfNodeId <- getSelfNode
    let nns = S.nodeNames $ S.nodes settings
        vns = M.delete selfNodeId $ M.map (const Nothing) nns
        pingPeriod = S.nodePingPeriodMilliseconds $ S.cluster settings
        state = State{
            registry=T.empty,
            pidIndex=M.empty,
            visibleNodes=vns,
            nodeNames=nns,
            eventPort=evPort
            }
    runPing pingPeriod $ M.keys vns
    void $ loop state


thereIsQuorum :: State -> Bool
thereIsQuorum state =
    let size = fromIntegral $ M.size (visibleNodes state) :: Float
        minCount = ceiling $ size / 2
        count = M.size $ M.filter isJust $ visibleNodes state
    in count >= minCount


getLocalRegistry :: State -> Process (T.Trie Record)
getLocalRegistry state = do
    selfNodeId <- fmap processNodeId getSelfPid
    let
        isLocal record@(_, pid) =
            if selfNodeId == processNodeId pid
                then Just record
                else Nothing
    return $ T.filterMap isLocal $ registry state


mergeRecord :: State -> B.ByteString -> Record -> Process State
mergeRecord state name record = do
    log Debug $ "Merge record: " ++ toString name
    case T.lookup name reg of
        Nothing -> do
            monitor pid
            return $ state{
                registry=T.insert name record reg,
                pidIndex=insertName pid name $ pidIndex state
                }
        Just record'@(ts', pid')
            | pid == pid' ->
                return state{
                    registry=T.insert name (min ts ts', pid) reg
                    }
            | record > record' -> do
                exit pid RegistrationFailure
                logConflict
                return state
            | otherwise -> do
                exit pid' RegistrationFailure
                logConflict
                monitor pid
                let modPids = insertName pid name . deleteName pid' name
                return $ state{
                    registry=T.insert name record reg,
                    pidIndex=modPids $ pidIndex state
                    }
    where
        reg = registry state
        (ts, pid) = record
        logConflict = log Info $ "Name conflict: " ++ toString name


loop :: State -> Process State
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        prepareCall h = callResponse (h state)
        handlers = [
            prepareCall handleWhereIs,
            prepareCall handleRegister,
            prepare handleMerge,
            prepare handleMonitorNotification,
            prepareCall handleGetNameList,
            prepareCall handleWhereIsByPrefix,
            prepare handlePing,
            prepareCall handleMergeRequest,
            prepare handleNodeMonitorNotification,
            prepareCall handleGetVisibleNodes,
            matchUnknown (return state)
            ]


handlePing :: State -> Ping -> Process State
handlePing state (Ping nodeId grPid) =
    case M.lookup nodeId vns of
        Just Nothing -> do
            monitorNode nodeId
            merge <- fmap Merge (getLocalRegistry state')
            send grPid merge
            log Info $ "New node: " ++ nodeName
            sendChan (eventPort state) (NewNode nodeId nodeName)
            when quorumAchieved $ log Info "Quorum achieved"
            return state'
        Just _ -> return state
        Nothing -> do
            log Error $ "Unknown node id: " ++ show nodeId
            return state
    where
        nodeName = nodeNames state M.! nodeId
        vns = visibleNodes state
        state' = state{visibleNodes=M.insert nodeId (Just grPid) vns}
        quorumAchieved = not (thereIsQuorum state) && thereIsQuorum state'


handleMerge :: State -> Merge -> Process State
handleMerge state (Merge reg) = foldM merge state $ T.toList reg
    where merge state' (name, record) = mergeRecord state' name record


handleMergeRequest :: State -> MergeRequest -> Process ((), State)
handleMergeRequest state (MergeRequest reg) = do
    state' <- handleMerge state (Merge reg)
    return ((), state')


handleRegister :: State -> Register -> Process (RegistrationResult, State)
handleRegister state (Register name pid async) = do
    ts <- liftIO milliseconds
    let n = fromString name
        reg = registry state
        record = (ts, pid)
        ok = thereIsQuorum state && not (n `T.member` reg)
        state' = state{
            registry=T.insert n record reg,
            pidIndex=insertName pid n $ pidIndex state
            }
        grPids = [grPid | Just grPid <- M.elems $ visibleNodes state']
        payload = T.singleton n record
    if ok
        then do
            monitor pid
            if async
                then do
                    mapM_ (\grPid -> send grPid (Merge payload)) grPids
                    return (Success, state')
                else return (Delegate grPids (MergeRequest payload), state')
        else return (Failure, state)


handleWhereIs :: State -> WhereIs -> Process ([Maybe ProcessId], State)
handleWhereIs state (WhereIs names) =
    let ns = map fromString names
        reg = registry state
        getPid n = snd <$> T.lookup n reg
    in return (map getPid ns, state)


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state (ProcessMonitorNotification _ pid _)
    | pid `M.member` pidIndex state = do
        let names = pidIndex state M.! pid
        log Debug $ "Unregister names: " ++ unwords (map toString names)
        return $ state{
            registry=foldl (flip T.delete) (registry state) names,
            pidIndex=M.delete pid $ pidIndex state
            }
    | otherwise = return state


handleNodeMonitorNotification ::
    State -> NodeMonitorNotification -> Process State
handleNodeMonitorNotification state notification = do
    let (NodeMonitorNotification _ nodeId _) = notification
        nodeName = nodeNames state M.! nodeId
        vns = visibleNodes state
        state' = state{visibleNodes=M.insert nodeId Nothing vns}
        noQuorum = not $ thereIsQuorum state'
        sendExit (_, localPid) = exit localPid RegistrationFailure
    log Info $ "Node disconnected: " ++ nodeName
    when noQuorum $ do
        localRegistry <- getLocalRegistry state'
        mapM_ sendExit $ T.elems localRegistry
        when (thereIsQuorum state) (log Info "Quorum lost")
    return state'


handleWhereIsByPrefix ::
    State -> WhereIsByPrefix -> Process ([ProcessId], State)
handleWhereIsByPrefix state (WhereIsByPrefix prefix) =
    let submap = T.submap (fromString prefix) (registry state)
    in return (map snd $ T.elems submap, state)


handleGetNameList :: State -> GetNameList -> Process (NameList, State)
handleGetNameList state (GetNameList prefix) = return (map f nameList, state)
    where
        nameList = T.toList $ T.submap (fromString prefix) (registry state)
        f (name, (ts, pid)) = (toString name, pid, ts)


handleGetVisibleNodes ::
    State -> GetVisibleNodes ->
    Process ((NodeName, M.Map NodeName Bool), State)
handleGetVisibleNodes state _ = do
    selfNodeId <- getSelfNode
    return ((name selfNodeId, M.map isJust (M.mapKeys name vns)), state)
    where
        name = (nodeNames state M.!)
        vns = visibleNodes state


runPing :: Ts -> [NodeId] -> Process ProcessId
runPing ms nodeIds = do
    selfPid <- getSelfPid
    selfNodeId <- getSelfNode
    let payload = Ping selfNodeId selfPid
        s nodeId = nsendRemote nodeId globalRegistryServiceName payload
        ping = forever $ sleepFor ms Millis >> mapM_ s nodeIds
    spawnLocal $ link selfPid >> ping


insertName :: ProcessId -> B.ByteString -> PidIndex -> PidIndex
insertName pid name index = index'
    where
        names = M.findWithDefault [] pid index
        index' = M.insert pid (name : names) index


deleteName :: ProcessId -> B.ByteString -> PidIndex -> PidIndex
deleteName pid name index = index'
    where
        names = M.findWithDefault [] pid index
        names' = delete name names
        index' =
            case names' of
                [] -> M.delete pid index
                _ -> M.insert pid names' index


log :: LogLevel -> String -> Process ()
log level txt = L.log level $ "GlobalRegistry - " ++ txt


request :: (Serializable a, Serializable b) => a -> TagPool -> Process b
request msg tagPool = do
    Just grPid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    Just res <- callTimeout grPid msg tag timeoutForCall
    return res


--external interface


isRunning :: Process Bool
isRunning = isJust <$> whereis globalRegistryServiceName


getNameList :: String -> TagPool -> Process NameList
getNameList prefix = request (GetNameList prefix)


getVisibleNodes :: TagPool -> Process (NodeName, M.Map NodeName Bool)
getVisibleNodes = request GetVisibleNodes


failure :: Process ()
failure = die RegistrationFailure


globalRegister :: String -> ProcessId -> TagPool -> Process ()
globalRegister name pid tagPool = do
    res <- request (Register name pid False) tagPool
    case res of
        Delegate grPids mergeReq -> do
            tag <- getTag tagPool
            res' <- multicall grPids mergeReq tag timeoutForCall
            let ok = length [True | Just () <- res'] == length grPids
            when (not ok) failure
        _ -> failure


globalRegisterAsync :: String -> ProcessId -> TagPool -> Process ()
globalRegisterAsync name pid tagPool = do
    res <- request (Register name pid True) tagPool
    case res of
        Success -> return ()
        _ -> failure


globalMultiWhereIs :: [String] -> TagPool -> Process [Maybe ProcessId]
globalMultiWhereIs names = request (WhereIs names)


globalWhereIs :: String -> TagPool -> Process (Maybe ProcessId)
globalWhereIs name tagPool = do
    [maybePid] <- globalMultiWhereIs [name] tagPool
    return maybePid


globalWhereIsByPrefix :: String -> TagPool -> Process [ProcessId]
globalWhereIsByPrefix prefix = request (WhereIsByPrefix prefix)


multicallByPrefix ::
    (Serializable a, Serializable b) => String -> a -> TagPool -> Process [b]
multicallByPrefix prefix msg tagPool = do
    pids <- globalWhereIsByPrefix prefix tagPool
    tag <- getTag tagPool
    res <- multicall pids msg tag timeoutForCall
    return [val | Just val <- res]


globalNSend :: Serializable a => String -> a -> Process ()
globalNSend name payload =
    void $ spawnLocal $ do
        maybePid <- globalWhereIs name =<< newTagPool
        case maybePid of
            Just pid -> send pid payload
            Nothing -> return ()


