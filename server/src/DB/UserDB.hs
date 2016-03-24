{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB.UserDB (userDBProcess, putUser, getUser) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (forever)
import Data.ByteString.Char8 (pack)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)
import Database.LevelDB.Base (
    DB, BatchOp(Put), get, write,
    defaultReadOptions, defaultWriteOptions
    )

import Base.GlobalRegistry (globalWhereIsByPrefix)
import Base.Logger (log)
import Types (
    UserId(..), ServiceId,
    ServiceType(UserDB), LogLevel(Error), prefix
    )
import Utils (safeReceive, timeoutForCall, milliseconds)
import User.Types (User, userId)
import DB.Utils (dbProcess, key)
import DB.Types (Persistent(fromByteString, toByteString))
import qualified DB.Settings as DS


data GetUser = GetUser UserId deriving (Generic, Typeable)

instance Binary GetUser


data PutUser = PutUser User deriving (Generic, Typeable)

instance Binary PutUser


updateTsKeyPrefix :: String
updateTsKeyPrefix = "update-timestamp:"


userDBProcess :: DS.Settings -> ServiceId -> Process ()
userDBProcess settings ident = dbProcess loop settings $ "user" ++ ident


loop :: DB -> Process ()
loop db = forever $ safeReceive handlers ()
    where
        prepare h = match (h db)
        prepareCall h = callResponse (h db)
        handlers = [
            prepare handlePutUser,
            prepareCall handleGetUser,
            matchUnknown (return ())
            ]


handleGetUser :: DB -> GetUser -> Process (Maybe User, ())
handleGetUser db (GetUser uid) = do
    res <- get db defaultReadOptions $ key uid
    return (res >>= Just . fromByteString, ())


handlePutUser :: DB -> PutUser -> Process ()
handlePutUser db (PutUser user) = do
    ts <- liftIO milliseconds
    let uid = userId user
        UserId ident = uid
        batch = [
            Put (key uid) (toByteString user),
            Put (key $ updateTsKeyPrefix ++ ident) (pack $ show ts)
            ]
    write db defaultWriteOptions batch


getDBForUser :: UserId -> TagPool -> Process ProcessId
getDBForUser (UserId uid) tagPool = do
    res <- globalWhereIsByPrefix (prefix UserDB) tagPool
    case res of
        [dbPid] -> return dbPid
        _ -> do
            log Error $ "Cannot get db for user " ++ uid
            terminate


--external interface


getUser :: UserId -> TagPool -> Process (Maybe User)
getUser uid tagPool = do
    pid <- getDBForUser uid tagPool
    tag <- getTag tagPool
    Just res <- callTimeout pid (GetUser uid) tag timeoutForCall
    return res


putUser :: User -> TagPool -> Process ()
putUser user tagPool = do
    spawnLocal $ do
        pid <- getDBForUser (userId user) tagPool
        send pid $ PutUser user
    return ()

