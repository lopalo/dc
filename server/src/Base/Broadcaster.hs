
module Base.Broadcaster (broadcasterProcess, isRunning) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)

import Control.Distributed.Process


broadcasterServiceName :: String
broadcasterServiceName = "broadcaster"


broadcasterProcess :: Process ()
broadcasterProcess = return ()


isRunning :: Process Bool
isRunning = isJust <$> whereis broadcasterServiceName

