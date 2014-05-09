module Main where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.WebSockets (runServer)
import Connection (acceptConnection)
import Controller (inputHandler)
import qualified Control.Distributed.Process.Node as Node

main :: IO ()
main = do
    Right transport <- createTransport "127.0.0.1" "10500" defaultTCPParameters
    node <- Node.newLocalNode transport Node.initRemoteTable
    runServer "127.0.0.1" 10501 $ acceptConnection node inputHandler





