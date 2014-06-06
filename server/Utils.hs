{-# LANGUAGE ScopedTypeVariables #-}

module Utils (delPrefix, milliseconds, logInfo, logError,
              logException, logDebug, evaluate) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Control.Exception as Ex
import Control.Monad (liftM, when)
import Data.String.Utils (split, join)

import Control.Distributed.Process

import qualified Settings as S


delPrefix :: String -> String -> String -> String
delPrefix delimiter prefix str =
    case delimiter `split` str of
        h:rest | h == prefix-> delimiter `join` rest

milliseconds :: IO Int
milliseconds = liftM (floor . (* 1000)) getPOSIXTime


logInfo :: String -> Process ()
logInfo = say . ("|INFO| " ++)

logError :: String -> Process ()
logError = say . ("|ERROR| " ++)

logDebug :: String -> Process ()
logDebug str = when S.debug $ say ("|DEBUG| " ++ str)

logException :: forall a. a -> [Handler a]
logException x = [Handler (\(ex :: Ex.PatternMatchFail) -> logEx ex),
                  Handler (\(ex :: Ex.ErrorCall) -> logEx ex),
                  Handler (\(ex :: Ex.IOException) -> logEx ex),
                  Handler (\(ex :: Ex.AssertionFailed) -> logEx ex),
                  Handler (\(ex :: Ex.ArithException) -> logEx ex)]
    where logEx :: Show b => b -> Process a
          logEx ex = logError (show ex) >> return x

evaluate :: a -> Process a
evaluate = liftIO . Ex.evaluate



