module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.WebSockets (runServer)
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import App.Connection (acceptConnection)
import App.Controller (inputHandler)
import App.Area.Area (areaProcess)
import App.DB (dbProcess)
import qualified App.Settings as S



start :: S.Settings -> Process ()
start settings = do
    spawnLocal (dbProcess settings)
    mapM_ startArea $ S.areas settings
    where
        startArea aid = spawnLocal $ areaProcess (S.area settings) aid


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
            Node.runProcess node $ start settings
            let (wsHost, wsPort) = S.wsAddress settings
                accept = acceptConnection node $ inputHandler settings
            runServer wsHost wsPort accept





