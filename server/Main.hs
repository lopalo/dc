module Main where

import Control.Monad (mapM_)

import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.WebSockets (runServer)
import qualified Control.Distributed.Process.Node as Node

import Connection (acceptConnection)
import Controller (inputHandler)
import Area.Area (areaProcess)
import qualified Settings as S



start :: Process ()
start = mapM_ startArea S.areas where
    startArea areaId = do
        areadPid <- spawnLocal $ areaProcess areaId
        register areaId areadPid


main :: IO ()
main = do
    Right transport <- createTransport "127.0.0.1" "10500" defaultTCPParameters
    node <- Node.newLocalNode transport Node.initRemoteTable
    Node.runProcess node start
    runServer "127.0.0.1" 10501 $ acceptConnection node inputHandler


--TODO: create cabal file



