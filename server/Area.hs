{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area (areas, startArea, areaProcess) where

import GHC.Generics (Generic)
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process

areas = ["start_area"]
startArea = "start_area"
tickMicroseconds = 1000000

data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick

scheduleTick :: Int -> Process ProcessId
scheduleTick microseconds = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO (threadDelay microseconds)
        send selfPid TimeTick

handleTick :: String -> TimeTick -> Process ()
handleTick name TimeTick = do
    say $ "handle tick of area: " ++ name
    scheduleTick tickMicroseconds
    return ()


areaProcess :: String -> Process ()
areaProcess name = do
    scheduleTick tickMicroseconds
    forever $ receiveWait [match (handleTick name)]

