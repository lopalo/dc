module Main where

import Control.Monad (forever, when)
import System.Environment (getArgs)
import Data.Map.Strict ((!))
import System.Random (randomRIO)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, newTagPool)
import Control.Distributed.Process.Extras.Time (milliSeconds)
import Control.Distributed.Process.Extras.Timer (sleep)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import Types (NodeName)
import Utils (sleepSeconds, logInfo, logDebug)
import Base.Broadcaster (broadcasterProcess)
import Base.GlobalCache (globalCacheProcess)
import Base.GlobalRegistry (globalRegistryProcess, globalRegister)
import Area.Area (areaProcess)
import DB.DB (dbProcess)
import HTTP.HTTP (httpProcess)
import WS.WS (wsProcess)
import Admin.Admin (adminProcess)
import NodeControl.NodeControl (nodeControlProcess)
import qualified Settings as S


startNode ::
    Node.LocalNode -> S.Settings -> NodeName -> Process ()
startNode node settings nodeName = do
    logInfo $ "Start node: " ++ nodeName
    let services = S.services $  S.nodes settings ! nodeName
        nodeService = S.NodeControl $ "node:" ++ nodeName
        services' = nodeService : services
    tagPool <- newTagPool
    spawnLocal broadcasterProcess
    spawnLocal globalCacheProcess
    spawnLocal $ globalRegistryProcess settings
    mapM_ (spawnService node settings tagPool) services'


spawnService ::
    Node.LocalNode -> S.Settings -> TagPool -> S.ServiceSettings -> Process ()
spawnService node settings tagPool serviceSettings = do
    let delayR = S.respawnDelayMilliseconds $ S.cluster settings
        randomDelay = liftIO $ randomRIO delayR
        ident = S.ident serviceSettings
        service =
            case serviceSettings of
                S.DB{S.path=path} -> dbProcess path
                S.WS{S.host=host, S.port=port} ->
                    wsProcess settings node host port
                S.HTTP{S.host=host, S.port=port} ->
                    httpProcess (S.http settings) host port
                S.Admin{S.host=host, S.port=port} ->
                    adminProcess settings node host port
                S.Area{} ->
                    areaProcess (S.area settings) (read ident)
                S.NodeControl{} -> nodeControlProcess
        wait = randomDelay >>= sleep . milliSeconds
        serviceLoop = do
            wait
            forever $ do
                --logDebug $ "Try to start service: " ++ ident
                pid <- getSelfPid
                ok <- globalRegister ident pid tagPool
                when ok $ do
                    logInfo $ "Start service: " ++ ident
                    service
                wait
        spawnLoop = spawnLocal $ serviceLoop `finally` spawnLoop
    spawnLoop
    return ()


main :: IO ()
main = do
    (settingsPath:nodeName:[]) <- getArgs
    res <-
        decodeFileEither settingsPath
        :: IO (Either ParseException S.Settings)
    case res of
        Left err -> print err
        Right settings -> do
            let nodeSettings = S.nodes settings ! nodeName
                host = S.nodeHost nodeSettings
                port = show $ S.nodePort nodeSettings
            Right transport <- createTransport host port defaultTCPParameters
            node <- Node.newLocalNode transport Node.initRemoteTable
            Node.runProcess node $ startNode node settings nodeName
            forever $ sleepSeconds 1

