module Main where

import Control.Monad (mapM_)
import Control.Distributed.Process
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.WebSockets (runServer)
import Connection (acceptConnection)
import Controller (inputHandler)
import Area (areas, areaProcess)
import qualified Control.Distributed.Process.Node as Node



start :: Process ()
start = mapM_ startArea areas where
    startArea name = do
        areadPid <- spawnLocal $ areaProcess name
        register name areadPid


main :: IO ()
main = do
    Right transport <- createTransport "127.0.0.1" "10500" defaultTCPParameters
    node <- Node.newLocalNode transport Node.initRemoteTable
    Node.runProcess node start
    runServer "127.0.0.1" 10501 $ acceptConnection node inputHandler





