
module WS.WS (wsProcess) where

import Control.Monad (forever)

import Control.Distributed.Process
import Control.Distributed.Process.Node (LocalNode)
import Control.Distributed.Process.Extras.Time (TimeUnit(..))
import Control.Distributed.Process.Extras.Timer (sleepFor)
import Network.WebSockets (runServer)

import Types (ServiceId)
import Base.GlobalCache (Value(WSAddress), set, durationFactor)
import WS.Connection (acceptConnection)
import WS.ClientCommands (inputHandler)
import qualified Settings as S


wsProcess ::
    S.Settings -> LocalNode -> ServiceId -> String -> Int -> Process ()
wsProcess settings node ident host port = do
    runPing ident host port
    liftIO $ runServer host' port accept
    where
        accept = acceptConnection node $ inputHandler settings
        host' = if host == "<http-host>" then "0.0.0.0" else host


runPing :: String -> String -> Int -> Process ProcessId
runPing key host port = do
    selfPid <- getSelfPid
    spawnLocal $ link selfPid >> ping
    where
        ping =
            forever $ do
                sleepFor periodSeconds Seconds
                set key value duration
        value = WSAddress host port
        periodSeconds = 3
        duration = periodSeconds * 1000 * durationFactor

