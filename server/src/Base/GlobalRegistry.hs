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


import Types (NodeName, Ts)
import qualified Settings as S
import Utils (
    safeReceive, timeoutForCall, milliseconds,
    logError, logInfo, logDebug
    )


type Record = (Ts, ProcessId)


data State = State {
    registry :: T.Trie Record,
    pidToNames :: M.Map ProcessId B.ByteString,
    nameServices :: M.Map NodeName (Maybe ProcessId)
    }


data Ping = Ping NodeName ProcessId deriving (Generic, Typeable)

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


globalRegistryProcess :: S.Settings -> NodeName -> Process ()
globalRegistryProcess settings nodeName = do
    register globalRegistryServiceName =<< getSelfPid
    let nodeIds = M.delete nodeName $ M.map S.nodeId $ S.nodes settings
        pingPeriod = S.nodePingPeriodMilliseconds $ S.cluster settings
        state = State{
            registry=T.empty,
            pidToNames=M.empty,
            nameServices=M.map (const Nothing) nodeIds
            }
    runPing pingPeriod nodeName $ M.elems nodeIds
    void $ loop state


isRunning :: Process Bool
isRunning = fmap isJust $ whereis globalRegistryServiceName


thereIsQuorum :: State -> Bool
thereIsQuorum state =
    let size = fromIntegral $ M.size (nameServices state) :: Float
        minCount = ceiling $ size / 2
        count = M.size $ M.filter isJust $ nameServices state
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
    logDebug $ "Merge record: " ++ toString name
    case T.lookup name reg of
        Nothing -> do
            monitor pid
            return $ state{
                registry=T.insert name record reg,
                pidToNames=M.insert pid name $ pidToNames state
                }
        Just record'@(_, pid')
            | record == record' -> return state
            | record > record' -> do
                exit pid RegistrationFailure
                logInfo $ "Remote name conflict: " ++ toString name
                return state
            | otherwise -> do
                exit pid' RegistrationFailure
                logInfo $ "Local name conflict: " ++ toString name
                monitor pid
                let modPids = M.insert pid name . M.delete pid'
                return $ state{
                    registry=T.insert name record reg,
                    pidToNames=modPids $ pidToNames state
                    }
    where
        reg = registry state
        (_, pid) = record


loop :: State -> Process State
loop state = safeReceive handlers state >>= loop
    where
        prepare h = match (h state)
        prepareCall h = callResponse (h state)
        handlers = [
            prepareCall handleWhereIs,
            prepareCall handleRegister,
            prepare handleMonitorNotification,
            prepare handleRemoteRegister,
            prepare handlePing,
            prepare handleMerge,
            prepareCall handleGetNameList,
            prepareCall handleWhereIsByPrefix,
            matchUnknown (return state)
            ]


handlePing :: State -> Ping -> Process State
handlePing state (Ping nodeName nsPid) =
    case M.lookup nodeName nss of
        Just Nothing -> do
            monitor nsPid
            fmap Merge (getLocalRegistry state') >>= send nsPid
            logInfo $ "New node: " ++ nodeName
            when quorumAchieved $ logInfo "Quorum achieved"
            return state'
        Just _ -> return state
        Nothing -> do
            logError $ "Unknown node: " ++ nodeName
            return state
    where
        nss = nameServices state
        state' = state{nameServices=M.insert nodeName (Just nsPid) nss}
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
            pidToNames=M.insert pid n $ pidToNames state
            }
        s (Just nsPid) = send nsPid $ RemoteRegister n record
        s Nothing = return ()
    when ok $ do
        monitor pid
        mapM_ s $ M.elems $ nameServices state'
    return $ if ok then (True, state') else (False, state)


handleRemoteRegister :: State -> RemoteRegister -> Process State
handleRemoteRegister state (RemoteRegister name record) =
    mergeRecord state name record


handleWhereIs :: State -> WhereIs -> Process ([Maybe ProcessId], State)
handleWhereIs state (WhereIs names) =
    let ns = map fromString names
        reg = registry state
        getPid n = fmap snd $ T.lookup n reg
    in return (map getPid ns, state)


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state (ProcessMonitorNotification _ pid _)
    | pid `M.member` pidToNames state = do
        let name = pidToNames state M.! pid
        logDebug $ "Unregister name: " ++ toString name
        return $ state{
            registry=T.delete name $ registry state,
            pidToNames=M.delete pid $ pidToNames state
            }
    | otherwise = do
        let f v@(Just pid') = if pid == pid' then Nothing else v
            f Nothing = Nothing
            nss = nameServices state
            nss' = M.map f nss
            state' = state{nameServices=nss'}
            noQuorum = not $ thereIsQuorum state'
            sendExit (_, localPid) = exit localPid RegistrationFailure
            [nodeName] = M.keys $ M.filter (Just pid ==) nss
        when (nss' /= nss) (logInfo $ "Node disconnected: " ++ nodeName)
        when noQuorum $ do
            localRegistry <- getLocalRegistry state'
            mapM_ sendExit $ T.elems localRegistry
            when (thereIsQuorum state) (logInfo "Quorum lost")
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


runPing :: Ts -> NodeName -> [NodeId] -> Process ProcessId
runPing ms nodeName nodeIds = do
    selfPid <- getSelfPid
    let payload = Ping nodeName selfPid
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


