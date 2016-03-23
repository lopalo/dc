
module DB.Utils (dbProcess, key) where

import Prelude hiding (log)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Control.Distributed.Process
import Database.LevelDB.Base (DB, open, defaultOptions)
import Database.LevelDB.Internal (unsafeClose)

import Base.Logger (log)
import qualified DB.Settings as DS
import Types (LogLevel(..))
import DB.Types (DBName)


dbProcess :: (DB -> Process()) -> DS.Settings -> DBName -> Process ()
dbProcess handler settings ident =
    bracket openDB closeDB handler `catch` errorHandler
    where
        path = intercalate "/" [DS.dbDir settings, ident]
        openDB = open path defaultOptions
        closeDB = liftIO . unsafeClose
        errorHandler e =
            log Error $ "DB error: " ++ show (e :: IOError)


key :: Show a => a -> ByteString
key = fromString . show

