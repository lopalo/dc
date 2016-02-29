
module Base.GlobalCache (globalCacheProcess, isRunning) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)

import Control.Distributed.Process


globalCacheServiceName :: String
globalCacheServiceName = "globalCache"


globalCacheProcess :: Process ()
globalCacheProcess = return ()


--external interface


isRunning :: Process Bool
isRunning = isJust <$> whereis globalCacheServiceName

