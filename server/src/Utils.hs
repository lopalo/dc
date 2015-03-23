{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Utils where

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Control.Exception as Ex
import Control.Monad (liftM, when)
import Data.String.Utils (split, join)

import Control.Distributed.Process
import Database.SQLite3 (SQLError)
import Database.SQLite.Simple (FormatError, ResultError)

import Debug (debug)

type Ts = Int -- time in milliseconds

delPrefix :: String -> String -> String -> String
delPrefix delimiter prefix str =
    case delimiter `split` str of
        h:rest | h == prefix -> delimiter `join` rest

milliseconds :: IO Ts
--TODO: use CLOCK_MONOTONIC
milliseconds = liftM (floor . (* 1000)) getPOSIXTime


logInfo :: String -> Process ()
logInfo = say . ("|INFO| " ++)

logError :: String -> Process ()
logError = say . ("|ERROR| " ++)

logDebug :: String -> Process ()
logDebug str = when debug $ say ("|DEBUG| " ++ str)

logException :: forall a. a -> [Handler a]
logException x = [Handler (\(ex :: Ex.PatternMatchFail) -> logEx ex),
                  Handler (\(ex :: Ex.ErrorCall) -> logEx ex),
                  Handler (\(ex :: Ex.IOException) -> logEx ex),
                  Handler (\(ex :: Ex.AssertionFailed) -> logEx ex),
                  Handler (\(ex :: Ex.ArithException) -> logEx ex),
                  Handler (\(ex :: SQLError) -> logEx ex),
                  Handler (\(ex :: FormatError) -> logEx ex),
                  Handler (\(ex :: ResultError) -> logEx ex)]
    where logEx :: Show b => b -> Process a
          logEx ex = logError (show ex) >> return x

evaluate :: a -> Process a
evaluate = liftIO . Ex.evaluate

safeReceive :: [Match a] -> a -> Process a
safeReceive handlers state = evalState `catches` logException state
    where evalState = receiveWait handlers >>= evaluate

--TODO: safe send
