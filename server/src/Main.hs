module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Network.WebSockets as WS
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import GlobalRegistry (globalRegistryProcess)
import Connection (acceptConnection)
import ClientCommands (inputHandler)
import Area.Area (areaProcess)
import DB (dbProcess)
import HTTPServer (httpServer)
import Admin.Admin (adminServer)
import qualified Settings as S

--TODO: move each service (process or server) to a separate directory with
--      custom Settings.hs

startServices :: Node.LocalNode -> S.Settings -> Process ()
startServices node settings = do
    --TODO: run inside supervisor
    spawnLocal globalRegistryProcess
    spawnLocal (wsServer settings node)
    spawnLocal (httpServer settings)
    spawnLocal (dbProcess settings)
    mapM_ startArea $ S.areas settings
    spawnLocal $ adminServer (S.admin settings) node
    return ()
    where
        startArea aid = spawnLocal $ areaProcess (S.area settings) aid


wsServer :: S.Settings -> Node.LocalNode -> Process ()
wsServer settings node = liftIO $ do
    let (wsHost, wsPort) = S.wsAddress settings
        accept = acceptConnection node $ inputHandler settings
        wsHost' = if wsHost == "<host>" then "0.0.0.0" else wsHost
    WS.runServer wsHost' wsPort accept


main :: IO ()
main = do
    (settingsPath:args) <- getArgs
    res <- decodeFileEither settingsPath
                :: IO (Either ParseException S.Settings)
    case res of
        Left err -> print err
        Right settings -> do
            let (nHost, nPort) = S.nodeAddress settings
            Right transport <- createTransport
                               nHost
                               nPort
                               defaultTCPParameters
            node <- Node.newLocalNode transport Node.initRemoteTable
            Node.runProcess node $ startServices node settings
            forever $ threadDelay 1000000

