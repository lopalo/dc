module Main where

import Control.Monad (mapM_)

import System.Environment (getArgs)
import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.WebSockets (runServer)
import qualified Control.Distributed.Process.Node as Node
import Data.Yaml (ParseException, decodeFileEither)

import Connection (acceptConnection)
import Controller (inputHandler)
import Area.Area (areaProcess)
import Settings (Settings, areas)



start :: Settings -> Process ()
start settings = mapM_ startArea $ areas settings where
    startArea areaId = do
        areadPid <- spawnLocal $ areaProcess settings areaId
        register areaId areadPid


main :: IO ()
main = do
    [settingsPath] <- getArgs
    res <- decodeFileEither settingsPath :: IO (Either ParseException Settings)
    case res of
        Left err -> print err
        Right settings -> do
            Right transport <- (createTransport
                                "127.0.0.1"
                                "10500"
                                defaultTCPParameters)
            node <- Node.newLocalNode transport Node.initRemoteTable
            Node.runProcess node $ start settings
            let accept = acceptConnection node $ inputHandler settings
            runServer "127.0.0.1" 10501 accept





