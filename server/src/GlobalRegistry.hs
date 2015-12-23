{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module GlobalRegistry (
    globalRegistryProcess,
    getRegistry, globalRegister,
    globalWhereIs, globalNSend
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callAt)

import Utils (safeReceive)

--TODO: use Data.Trie for fetching by prefix
type Registry = M.Map ProcessId String


data Register = Register String ProcessId deriving (Generic, Typeable)

instance Binary Register


data GetRegistry = GetRegistry deriving (Generic, Typeable)

instance Binary GetRegistry


globalRegistryServiceName :: String
globalRegistryServiceName = "globalRegistry"


globalRegistryProcess :: Process ()
globalRegistryProcess = do
    --TODO: replace this dummy server with a real global server
    register globalRegistryServiceName =<< getSelfPid
    void $ loop M.empty


loop :: Registry -> Process Registry
loop registry = safeReceive handlers registry >>= loop
    where
        prepare h = match (h registry)
        prepareCall h = callResponse (h registry)
        handlers = [
            prepare handleRegister,
            prepare handleMonitorNotification,
            prepareCall handleGetRegistry
            ]


handleRegister :: Registry -> Register -> Process Registry
handleRegister registry (Register name pid) = do
    monitor pid
    return $ M.insert pid name registry


handleMonitorNotification ::
    Registry -> ProcessMonitorNotification -> Process Registry
handleMonitorNotification registry (ProcessMonitorNotification _ pid _) =
    return $ M.delete pid registry


handleGetRegistry
    :: Registry -> GetRegistry -> Process (Maybe Registry, Registry)
handleGetRegistry registry GetRegistry = return (Just registry, registry)


--external interface

getRegistry :: TagPool -> Process (Maybe Registry)
getRegistry tagPool = do
    Just pid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    res <- callAt pid GetRegistry tag
    return $ fromMaybe Nothing res


globalRegister :: String -> ProcessId -> Process ()
globalRegister name pid = do
    register name pid
    nsend globalRegistryServiceName $ Register name pid


globalWhereIs :: String -> Process (Maybe ProcessId)
globalWhereIs = whereis


globalNSend :: Serializable a => String -> a -> Process ()
globalNSend name payload =
    void $ spawnLocal $ do
        maybePid <- globalWhereIs name
        case maybePid of
            Just pid -> send pid payload
            Nothing -> return ()

