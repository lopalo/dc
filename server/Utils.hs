module Utils (delPrefix, milliseconds, logInfo, logError, logDebug) where

import Data.Time.Clock.POSIX (getPOSIXTime)
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


