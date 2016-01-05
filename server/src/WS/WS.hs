
module WS.WS (wsProcess) where

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Network.WebSockets (runServer)

import WS.Connection (acceptConnection)
import WS.ClientCommands (inputHandler)
import qualified Settings as S


wsProcess :: S.Settings -> LocalNode -> String -> Int -> Process ()
wsProcess settings node host port = liftIO $ runServer host port accept
    where accept = acceptConnection node $ inputHandler settings



