
module DB.Utils (baseDbProcess, log) where

import Prelude hiding (log)
import Data.List (intercalate)
import Control.Monad.Catch (catch)

import Control.Distributed.Process hiding (catch)
import Database.LevelDB.Base (DB, withDB, defaultOptions)

import qualified Base.Logger as L
import qualified DB.Settings as DS
import Types (LogLevel(..))
import DB.Types (DBName)


baseDbProcess :: (DB -> Process()) -> DS.Settings -> DBName -> Process ()
baseDbProcess handler settings ident =
    withDB path defaultOptions handler `catch` errorHandler
    where
        path = intercalate "/" [DS.dbDir settings, ident]
        errorHandler e =
            log Error $ "Error: " ++ show (e :: IOError)


log :: LogLevel -> String -> Process ()
log level txt = L.log level $ "DB - " ++ txt

