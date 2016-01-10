{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module GlobalRegistry (
    RegistrationFailure(RegistrationFailure),
    globalRegistryProcess,
    getRegistry, globalRegister,
    globalWhereIs, globalNSend
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (void, when, forever, foldM)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Maybe (isJust)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Trie as T

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, newTagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)

import Types (NodeName, Ts)
import qualified Settings as S
import Utils (safeReceive, timeoutForCall, milliseconds, logError, logInfo)


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


data WhereIs = WhereIs String deriving (Generic, Typeable)

instance Binary WhereIs


data GetRegistry = GetRegistry String deriving (Generic, Typeable)

instance Binary GetRegistry


data RegistrationFailure =
    RegistrationFailure
    deriving (Generic, Typeable, Show)

instance Binary RegistrationFailure


type NameList = [(String, ProcessId)]


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
mergeRecord state name record =
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
                logConflict
                return state
            | otherwise -> do
                exit pid' RegistrationFailure
                logConflict
                monitor pid
                let modPids = M.insert pid name . M.delete pid'
                return $ state{
                    registry=T.insert name record reg,
                    pidToNames=modPids $ pidToNames state
                    }
    where
        reg = registry state
        (_, pid) = record
        logConflict = logInfo $ "Name conflict: " ++ toString name


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
            prepareCall handleGetRegistry,
            matchUnknown (return state)
            ]


handlePing :: State -> Ping -> Process State
handlePing state (Ping nodeName nsPid) =
    case M.lookup nodeName nss of
        Just Nothing -> do
            monitor nsPid
            fmap Merge (getLocalRegistry state') >>= send nsPid
            return state'
        Just _ -> return state
        Nothing -> do
            logError $ "Unknown node: " ++ nodeName
            return state
    where
        nss = nameServices state
        state' = state{nameServices=M.insert nodeName (Just nsPid) nss}


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


handleWhereIs :: State -> WhereIs -> Process (Maybe ProcessId, State)
handleWhereIs state (WhereIs name) = return $ (fmap snd maybeRecord, state)
    where maybeRecord = T.lookup (fromString name) (registry state)


handleMonitorNotification ::
    State -> ProcessMonitorNotification -> Process State
handleMonitorNotification state (ProcessMonitorNotification _ pid _)
    | pid `M.member` pidToNames state = do
        let name = pidToNames state M.! pid
        return $ state{
            registry=T.delete name $ registry state,
            pidToNames=M.delete pid $ pidToNames state
            }
    | otherwise = do
        let nss = nameServices state
            state' = state{nameServices=M.filter (Just pid /=) nss}
            noQuorum = not $ thereIsQuorum state'
            sendExit (_, localPid) = exit localPid RegistrationFailure
        when noQuorum $ do
            localRegistry <- getLocalRegistry state'
            mapM_ sendExit $ T.elems localRegistry
            logInfo "No quorum"
        return state'


handleGetRegistry :: State -> GetRegistry -> Process (NameList, State)
handleGetRegistry state (GetRegistry prefix) = return (map f nameList, state)
    where
        nameList = T.toList $ T.submap (fromString prefix) (registry state)
        f (name, (_, pid)) = (toString name, pid)


runPing :: Ts -> NodeName -> [NodeId] -> Process ProcessId
runPing ms nodeName nodeIds = do
    selfPid <- getSelfPid
    let time = ms * 1000
        payload = Ping nodeName selfPid
        s nodeId = nsendRemote nodeId globalRegistryServiceName payload
        ping =
            forever $ do
                liftIO $ threadDelay time
                mapM_ s nodeIds
    spawnLocal $ link selfPid >> ping


--external interface

getRegistry :: String -> TagPool -> Process NameList
getRegistry prefix tagPool = do
    Just regPid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    Just res <- callTimeout regPid (GetRegistry prefix) tag timeoutForCall
    return res


globalRegister :: String -> ProcessId -> TagPool -> Process Bool
globalRegister name pid tagPool = do
    Just regPid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    Just ok <- callTimeout regPid (Register name pid) tag timeoutForCall
    return ok


globalWhereIs :: String -> TagPool -> Process (Maybe ProcessId)
globalWhereIs name tagPool = do
    Just regPid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    Just maybePid <- callTimeout regPid (WhereIs name) tag timeoutForCall
    return maybePid


globalNSend :: Serializable a => String -> a -> Process ()
globalNSend name payload =
    void $ spawnLocal $ do
        maybePid <- globalWhereIs name =<< newTagPool
        case maybePid of
            Just pid -> send pid payload
            Nothing -> return ()



--TODO: more logging
