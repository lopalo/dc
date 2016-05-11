module Utils where

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Control.Exception as Ex
import Control.Monad (liftM)
import System.Random (RandomGen, StdGen, mkStdGen, randomR)
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar)

import Data.Hashable (hash)
import Control.Distributed.Process
import Control.Distributed.Process.Extras.Time (Timeout)
import Control.Distributed.Process.Node (LocalNode, runProcess)
import Web.Scotty hiding (settings)

import Types(Ts)
import Base.Logger (logException)


timeoutForCall :: Timeout
timeoutForCall = Just (seconds * 1000 * 1000)
    where seconds = 3


milliseconds :: IO Ts
milliseconds = liftM (floor . (* 1000)) getPOSIXTime


mkRandomGen :: Show a => a -> Int -> StdGen
mkRandomGen s int = mkStdGen $ hash (show s) + int


choice :: RandomGen g => [a] -> g -> (a, g)
choice lst g =
    let (idx, g') = randomR (0, length lst - 1) g
    in (lst !! idx, g')


evaluate :: a -> Process a
evaluate = liftIO . Ex.evaluate


safeReceive :: [Match a] -> a -> Process a
safeReceive handlers state = evalState `catches` logException state
    where evalState = receiveWait handlers >>= evaluate


sleepSeconds :: Int -> IO ()
sleepSeconds seconds = threadDelay $ seconds * 1000 * 1000


execProcess :: LocalNode -> Process a -> ActionM a
execProcess node proc =
    liftIO $ do
        var <- newEmptyMVar
        runProcess node $ do
            res <- proc
            liftIO $ putMVar var res
        takeMVar var

