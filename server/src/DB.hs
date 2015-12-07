{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB (dbProcess, putUser, getUser, getAreaObjects, putAreaObjects) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callAt)
import Database.SQLite.Simple (ToRow, SQLData, Only(..), Connection,
                               open, close, query, execute, toRow,
                               withTransaction)
import Database.SQLite.Simple.ToField (toField)

import GlobalRegistry (globalRegister, globalWhereIs, globalNSend)
import Types (UserId(..), AreaId(..))
import Utils (safeReceive)
import User.Types (User)
import Area.Objects.Gate (Gate)
import Area.Objects.Asteroid (Asteroid)
import qualified Settings as S


type AreaObjects = ([Gate], [Asteroid])

data GetUser = GetUser UserId deriving (Generic, Typeable)
instance Binary GetUser


data PutUser = PutUser User deriving (Generic, Typeable)
instance Binary PutUser


data GetAreaObjects = GetAreaObjects AreaId deriving (Generic, Typeable)
instance Binary GetAreaObjects


data PutAreaObjects = PutAreaObjects AreaId AreaObjects
                      deriving (Generic, Typeable)
instance Binary PutAreaObjects


dbServiceName :: String
dbServiceName = "db"

dbProcess :: S.Settings -> Process ()
dbProcess settings = do
    globalRegister dbServiceName =<< getSelfPid
    conn <- liftIO $ open $ S.db settings
    loop conn
    liftIO $ close conn


loop :: Connection -> Process ()
loop conn = forever $ safeReceive handlers ()
    where
        prepare h = match (h conn)
        prepareCall h = callResponse (h conn)
        handlers = [prepare handlePutUser,
                    prepare handlePutAreaObjects,
                    prepareCall handleGetUser,
                    prepareCall handleGetAreaObjects]

handleGetUser :: Connection -> GetUser -> Process (Maybe User, ())
handleGetUser conn (GetUser (UserId uid)) = do
    res <- liftIO $ query conn "SELECT * FROM user WHERE id = ?" (Only uid)
    case res of
        [user] -> return (Just user, ())
        [] -> return (Nothing, ())


handleGetAreaObjects :: Connection -> GetAreaObjects
                        -> Process (AreaObjects, ())
handleGetAreaObjects conn (GetAreaObjects (AreaId aid)) = do
    gates <- liftIO $ query conn "SELECT * FROM gate WHERE area = ?" (Only aid)
    asteroids <- liftIO $ query conn "SELECT * FROM asteroid WHERE area = ?"
                                                                (Only aid)
    return ((gates, asteroids), ())



handlePutUser :: Connection -> PutUser -> Process ()
handlePutUser conn (PutUser user) = do
    liftIO $ execute conn q user
    return ()
    where
        q = "INSERT OR REPLACE INTO user VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"


handlePutAreaObjects :: Connection -> PutAreaObjects -> Process ()
handlePutAreaObjects conn (PutAreaObjects (AreaId area) objects) =
    liftIO $ withTransaction conn transaction
    where
        gateQ = "INSERT OR REPLACE INTO gate VALUES \
                 \ (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        asteroidQ = "INSERT OR REPLACE INTO asteroid VALUES \
                     \ (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        addArea :: ToRow a => a -> [SQLData]
        addArea = ([toField area] ++) . toRow
        (gates, asteroids) = objects
        transaction = do
            mapM_ (execute conn gateQ . addArea) gates
            mapM_ (execute conn asteroidQ . addArea) asteroids


--external interface

getUser :: UserId -> TagPool -> Process (Maybe User)
getUser uid tagPool = do
    Just pid <- globalWhereIs dbServiceName
    tag <- getTag tagPool
    res <- callAt pid (GetUser uid) tag
    return $ fromMaybe Nothing res

putUser :: User -> Process ()
putUser user = globalNSend dbServiceName $ PutUser user

getAreaObjects :: AreaId -> TagPool -> Process (Maybe AreaObjects)
getAreaObjects aid tagPool = do
    Just pid <- globalWhereIs dbServiceName
    tag <- getTag tagPool
    callAt pid (GetAreaObjects aid) tag


putAreaObjects :: AreaId -> AreaObjects -> Process ()
putAreaObjects aid objs = globalNSend dbServiceName $ PutAreaObjects aid objs

