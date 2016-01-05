module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Data.Map.Strict ((!))

import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import GlobalRegistry (globalRegistryProcess)
import Area.Area (areaProcess)
import DB.DB (dbProcess)
import HTTP.HTTP (httpProcess)
import WS.WS (wsProcess)
import Admin.Admin (adminProcess)
import qualified Settings as S


startServices ::
    Node.LocalNode -> S.Settings -> [S.ServiceSettings] -> Process ()
startServices node settings services = do
    --TODO: run inside supervisor
    spawnLocal globalRegistryProcess
    mapM_ spawnService services
    return ()
    where
        spawnService service =
            spawnLocal $ do
                let ident = S.ident service
                --TODO: register
                --TODO: catch ProcessExitException from name server
                case service of
                    S.DB{S.path=path} -> dbProcess path
                    S.WS{S.host=host, S.port=port} ->
                        wsProcess settings node host port
                    S.HTTP{S.host=host, S.port=port} ->
                        httpProcess (S.http settings) host port
                    S.Admin{S.host=host, S.port=port} ->
                        adminProcess (S.admin settings) node host port
                    S.Area{} ->
                        areaProcess (S.area settings) (read ident)


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
                servicesSetting = S.services nodeSettings
            Right transport <- createTransport host port defaultTCPParameters
            node <- Node.newLocalNode transport Node.initRemoteTable
            Node.runProcess node $ startServices node settings servicesSetting
            forever $ threadDelay 1000000

