module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Network.WebSockets as WS
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import App.Connection (acceptConnection)
import App.Controller (inputHandler)
import App.Area.Area (areaProcess)
import App.DB (dbProcess)
import App.HTTPServer (httpServer)
import qualified App.Settings as S


startServices :: S.Settings -> Node.LocalNode -> Process ()
startServices settings node = do
    --TODO: run inside supervisor
    spawnLocal (wsServer settings node)
    spawnLocal (httpServer settings)
    spawnLocal (dbProcess settings)
    mapM_ startArea $ S.areas settings
    where
        startArea aid = spawnLocal $ areaProcess (S.area settings) aid


wsServer :: S.Settings -> Node.LocalNode -> Process ()
wsServer settings node = liftIO $ do
    let (wsHost, wsPort) = S.wsAddress settings
        accept = acceptConnection node $ inputHandler settings
    WS.runServer wsHost wsPort accept


main :: IO ()
main = do
    [settingsPath] <- getArgs
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
            Node.runProcess node $ startServices settings node
            forever $ threadDelay 1000000

