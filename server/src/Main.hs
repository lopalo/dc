module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Network.WebSockets as WS
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import App.GlobalRegistry (globalRegistryProcess)
import App.Connection (acceptConnection)
import App.ClientCommands (inputHandler)
import App.Area.Area (areaProcess)
import App.DB (dbProcess)
import App.HTTPServer (httpServer)
import Admin.Admin (adminServer)
import qualified App.Settings as S
import qualified Admin.Settings as AS


startServices :: Node.LocalNode -> S.Settings -> Maybe AS.Settings -> Process ()
startServices node settings maybeAdminSettings = do
    --TODO: run inside supervisor
    spawnLocal globalRegistryProcess
    spawnLocal (wsServer settings node)
    spawnLocal (httpServer settings)
    spawnLocal (dbProcess settings)
    mapM_ startArea $ S.areas settings
    case maybeAdminSettings of
        Just adminSettings -> do
            spawnLocal (adminServer adminSettings node)
            say "Admin started"
        Nothing -> return ()
    where
        startArea aid = spawnLocal $ areaProcess (S.area settings) aid


wsServer :: S.Settings -> Node.LocalNode -> Process ()
wsServer settings node = liftIO $ do
    let (wsHost, wsPort) = S.wsAddress settings
        accept = acceptConnection node $ inputHandler settings
        wsHost' = if wsHost == "<host>" then "0.0.0.0" else wsHost
    WS.runServer wsHost' wsPort accept


loadAdminSettings :: [String] -> IO (Maybe AS.Settings)
loadAdminSettings [] = return Nothing
loadAdminSettings [settingsPath] = do
    res <- decodeFileEither settingsPath
                :: IO (Either ParseException AS.Settings)
    case res of
        Left err -> print err >> return Nothing
        Right settings -> return $ Just settings

main :: IO ()
main = do
    (settingsPath:args) <- getArgs
    res <- decodeFileEither settingsPath
                :: IO (Either ParseException S.Settings)
    maybeAdminSettings <- loadAdminSettings args
    case res of
        Left err -> print err
        Right settings -> do
            let (nHost, nPort) = S.nodeAddress settings
            Right transport <- createTransport
                               nHost
                               nPort
                               defaultTCPParameters
            node <- Node.newLocalNode transport Node.initRemoteTable
            Node.runProcess node $
                startServices node settings maybeAdminSettings
            forever $ threadDelay 1000000

