{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module App.GlobalRegistry (globalRegistryProcess,
                           getAllNames, globalRegister) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callAt)

import App.Utils (safeReceive)


type Registry = M.Map ProcessId String


data Register = Register String ProcessId deriving (Generic, Typeable)
instance Binary Register


data GetAllNames = GetAllNames deriving (Generic, Typeable)
instance Binary GetAllNames


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
        handlers = [prepare handleRegister,
                    prepare handleMonitorNotification,
                    prepareCall handleGetAllNames]


handleRegister :: Registry -> Register -> Process Registry
handleRegister registry (Register name pid) = do
    monitor pid
    return $ M.insert pid name registry


handleMonitorNotification :: Registry -> ProcessMonitorNotification
                             -> Process Registry
handleMonitorNotification registry (ProcessMonitorNotification _ pid _) =
    return $ M.delete pid registry


handleGetAllNames :: Registry -> GetAllNames
                     -> Process (Maybe [String], Registry)
handleGetAllNames registry GetAllNames =
    return (Just (M.elems registry), registry)


--external interface

getAllNames :: TagPool -> Process (Maybe [String])
getAllNames tagPool = do
    Just pid <- whereis globalRegistryServiceName
    tag <- getTag tagPool
    res <- callAt pid GetAllNames tag
    return $ fromMaybe Nothing res

globalRegister :: String -> ProcessId -> Process ()
globalRegister name pid = do
    register name pid
    nsend globalRegistryServiceName $ Register name pid

