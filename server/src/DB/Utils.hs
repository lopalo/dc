
module DB.Utils (dbProcess, key) where

import Prelude hiding (log)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Control.Monad.Catch (catch)

import Control.Distributed.Process hiding (catch)
import Database.LevelDB.Base (DB, withDB, defaultOptions)

import Base.Logger (log)
import qualified DB.Settings as DS
import Types (LogLevel(..))
import DB.Types (DBName)


dbProcess :: (DB -> Process()) -> DS.Settings -> DBName -> Process ()
dbProcess handler settings ident =
    withDB path defaultOptions handler `catch` errorHandler
    where
        path = intercalate "/" [DS.dbDir settings, ident]
        errorHandler e =
            log Error $ "DB error: " ++ show (e :: IOError)


key :: Show a => a -> ByteString
key = fromString . show

