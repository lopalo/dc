{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Base.GlobalRegistry (
    RegistrationFailure(RegistrationFailure),
    globalRegistryProcess, isRunning,
    getNameList, globalRegister, globalWhereIs,
    globalMultiWhereIs, globalWhereIsByPrefix, globalNSend
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Applicative ((<$>))
import Control.Monad (void, when, forever, foldM)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Maybe (isJust)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Trie as T

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, newTagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)


import Types (NodeNames, Ts, LogLevel(..))
import qualified Settings as S
import Utils (safeReceive, timeoutForCall, milliseconds)
import Base.Logger (log)


type Record = (Ts, ProcessId)


data State = State {
    registry :: T.Trie Record,
    pidIndex :: M.Map ProcessId B.ByteString,
    visibleNodes :: M.Map NodeId Bool,
    nodeNames :: NodeNames
    }


data Ping = Ping NodeId deriving (Generic, Typeable)

instance Binary Ping


data Merge = Merge (T.Trie Record) deriving (Generic, Typeable)

instance Binary Merge


data RemoteRegister =
    RemoteRegister B.ByteString Record
    deriving (Generic, Typeable)

instance Binary RemoteRegister


data Register = Register String ProcessId deriving (Generic, Typeable)

instance Binary Register


data WhereIs = WhereIs [String] deriving (Generic, Typeable)

instance Binary WhereIs


data WhereIsByPrefix = WhereIsByPrefix String deriving (Generic, Typeable)

instance Binary WhereIsByPrefix


data GetNameList = GetNameList String deriving (Generic, Typeable)

instance Binary GetNameList


data RegistrationFailure =
    RegistrationFailure
    deriving (Generic, Typeable, Show)

instance Binary RegistrationFailure


type NameList = [(String, ProcessId, Ts)]


globalRegistryServiceName :: String
globalRegistryServiceName = "globalRegistry"


globalRegistryProcess :: S.Settings -> Process ()
globalRegistryProcess settings = do
    register globalRegistryServiceName =<< getSelfPid
    selfNodeId <- getSelfNode
    let nns = S.nodeNames $ S.nodes settings
        vns = M.delete selfNodeId $ M.map (const False) nns
        pingPeriod = S.nodePingPeriodMilliseconds $ S.cluster settings
        state = State{
            registry=T.empty,
            pidIndex=M.empty,
            visibleNodes=vns,
            nodeNames=nns
            }
    runPing pingPeriod $ M.keys vns
    void $ loop state


thereIsQuorum :: State -> Bool
thereIsQuorum state =
    let size = fromIntegral $ M.size (visibleNodes state) :: Float
        minCount = ceiling $ size / 2
        count = M.size $ M.filter id $ visibleNodes state
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
                pidIndex=M.insert pid name $ pidIndex state
                }
        Just record'@(ts', pid')
            | pid == pid' ->
                return state{
                    registry=T.insert name (min ts ts', pid) reg
                    }
            | record > record' -> do
                exit pid RegistrationFailure
                log Info $ "Remote name conflict: " ++ toString name
                return state
            | otherwise -> do
                exit pid' RegistrationFailure
                log Info $ "Local name conflict: " ++ toString name
                monitor pid
                let modPids = M.insert pid name . M.delete pid'
                return $ state{
                    registry=T.insert name record reg,
                    pidIndex=modPids $ pidIndex state
                    }
    where
        reg = registry state
        (ts, pid) = record


loop :: State -> Process State
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        prepareCall h = callResponse (h state)
        handlers = [
            prepareCall handleWhereIs,
            prepareCall handleRegister,
            prepare handleRemoteRegister,
            prepare handleMonitorNotification,
            prepareCall handleGetNameList,
            prepareCall handleWhereIsByPrefix,
            prepare handlePing,
            prepare handleMerge,
            prepare handleNodeMonitorNotification,
            matchUnknown (return state)
            ]


handlePing :: State -> Ping -> Process State
handlePing state (Ping nodeId) =
    case M.lookup nodeId vns of
        Just False -> do
            monitorNode nodeId
            merge <- fmap Merge (getLocalRegistry state')
            nsendRemote nodeId globalRegistryServiceName merge
            log Info $ "New node: " ++ nodeName
            when quorumAchieved $ log Info "Quorum achieved"
            return state'
        Just True -> return state
        Nothing -> do
            log Error $ "Unknown node id: " ++ show nodeId
            return state
    where
        nodeName = nodeNames state M.! nodeId
        vns = visibleNodes state
        state' = state{visibleNodes=M.insert nodeId True vns}
        quorumAchieved = not (thereIsQuorum state) && thereIsQuorum state'


handleMerge :: State -> Merge -> Process State
handleMerge state (Merge reg) = foldM merge state $ T.toList reg
    where merge state' (name, record) = mergeRecord state' name record


handleRegister :: State -> Register -> Process (Bool, State)
handleRegister state (Register name pid) = do
    ts <- liftIO milliseconds
    let n = fromString name
        reg = registry state
        record = (ts, pid)
        ok = thereIsQuorum state && not (n `T.member` reg)
        state' = state{
            registry=T.insert n record reg,
            pidIndex=M.insert pid n $ pidIndex state
            }
        payload = RemoteRegister n record
        s (nodeId, True) =
            nsendRemote nodeId globalRegistryServiceName payload
        s _ = return ()
    when ok $ do
        monitor pid
        mapM_ s $ M.toList $ visibleNodes state'
    return $ if ok then (True, state') else (False, state)


handleRemoteRegister :: State -> RemoteRegister -> Process State
handleRemoteRegister state (RemoteRegister name record) =
    mergeRecord state name record


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
        let name = pidIndex state M.! pid
        log Debug $ "Unregister name: " ++ toString name
        return $ state{
            registry=T.delete name $ registry state,
            pidIndex=M.delete pid $ pidIndex state
            }
    | otherwise = return state


handleNodeMonitorNotification ::
    State -> NodeMonitorNotification -> Process State
handleNodeMonitorNotification state notification = do
    let (NodeMonitorNotification _ nodeId _) = notification
        nodeName = nodeNames state M.! nodeId
        vns = visibleNodes state
        state' = state{visibleNodes=M.insert nodeId False vns}
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


runPing :: Ts -> [NodeId] -> Process ProcessId
runPing ms nodeIds = do
    selfPid <- getSelfPid
    selfNodeId <- getSelfNode
    let payload = Ping selfNodeId
        s nodeId = nsendRemote nodeId globalRegistryServiceName payload
        ping = forever $ sleepFor ms Millis >> mapM_ s nodeIds
    spawnLocal $ link selfPid >> ping


request :: (Serializable a, Serializable b) => a -> TagPool -> Process b
request msg tagPool = do
    Just regPid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    Just res <- callTimeout regPid msg tag timeoutForCall
    return res


--external interface


isRunning :: Process Bool
isRunning = isJust <$> whereis globalRegistryServiceName


getNameList :: String -> TagPool -> Process NameList
getNameList prefix = request (GetNameList prefix)


globalRegister :: String -> ProcessId -> TagPool -> Process Bool
globalRegister name pid = request (Register name pid)


globalMultiWhereIs :: [String] -> TagPool -> Process [Maybe ProcessId]
globalMultiWhereIs names = request (WhereIs names)


globalWhereIs :: String -> TagPool -> Process (Maybe ProcessId)
globalWhereIs name tagPool = do
    [maybePid] <- globalMultiWhereIs [name] tagPool
    return maybePid


globalWhereIsByPrefix :: String -> TagPool -> Process [ProcessId]
globalWhereIsByPrefix prefix = request (WhereIsByPrefix prefix)


globalNSend :: Serializable a => String -> a -> Process ()
globalNSend name payload =
    void $ spawnLocal $ do
        maybePid <- globalWhereIs name =<< newTagPool
        case maybePid of
            Just pid -> send pid payload
            Nothing -> return ()


