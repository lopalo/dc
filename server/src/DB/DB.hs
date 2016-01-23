{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB.DB (
    AreaObjects(..),
    dbProcess,
    putUser, getUser,
    getAreaObjects, putAreaObjects,
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)
import Database.SQLite.Simple (
    ToRow, SQLData, Only(..), Connection,
    open, close, query, execute, toRow,
    withTransaction
    )
import Database.SQLite.Simple.ToField (toField)

import Base.GlobalRegistry (globalWhereIs, globalNSend)
import Types (UserId(..), AreaId(..))
import Utils (safeReceive, timeoutForCall)
import User.Types (User)
import Area.Objects.Gate (Gate)
import Area.Objects.Asteroid (Asteroid)
import Area.Objects.ControlPoint (ControlPoint)


data AreaObjects = AreaObjects {
    gates :: ![Gate],
    asteroids :: ![Asteroid],
    controlPoints :: ![ControlPoint]
    }
    deriving (Generic, Typeable)

instance Binary AreaObjects


data GetUser = GetUser UserId deriving (Generic, Typeable)

instance Binary GetUser


data PutUser = PutUser User deriving (Generic, Typeable)

instance Binary PutUser


data GetAreaObjects = GetAreaObjects AreaId deriving (Generic, Typeable)

instance Binary GetAreaObjects


data PutAreaObjects =
    PutAreaObjects AreaId AreaObjects
    deriving (Generic, Typeable)

instance Binary PutAreaObjects


dbServiceName :: String
dbServiceName = "db"


dbProcess :: String -> Process ()
dbProcess path = do
    conn <- liftIO $ open $ path
    loop conn `finally` liftIO (close conn)


loop :: Connection -> Process ()
loop conn = forever $ safeReceive handlers ()
    where
        prepare h = match (h conn)
        prepareCall h = callResponse (h conn)
        handlers = [
            prepare handlePutUser,
            prepare handlePutAreaObjects,
            prepareCall handleGetUser,
            prepareCall handleGetAreaObjects,
            matchUnknown (return ())
            ]


handleGetUser :: Connection -> GetUser -> Process (Maybe User, ())
handleGetUser conn (GetUser (UserId uid)) = do
    res <- liftIO $ query conn "SELECT * FROM user WHERE id = ?" (Only uid)
    case res of
        [user] -> return (Just user, ())
        [] -> return (Nothing, ())


handleGetAreaObjects ::
    Connection -> GetAreaObjects -> Process (AreaObjects, ())
handleGetAreaObjects conn (GetAreaObjects (AreaId aid)) = do
    gates_ <-
        liftIO $
            query conn "SELECT * FROM gate WHERE area = ?" (Only aid)
    asteroids_ <-
        liftIO $
            query conn "SELECT * FROM asteroid WHERE area = ?" (Only aid)
    controlPoints_ <-
        liftIO $
            query
            conn
            "SELECT * FROM control_point WHERE area = ?" (Only aid)
    let objects = AreaObjects gates_ asteroids_ controlPoints_
    return (objects, ())


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
        gateQ =
            "INSERT OR REPLACE INTO gate VALUES \
             \ (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        asteroidQ =
            "INSERT OR REPLACE INTO asteroid VALUES \
             \ (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        controlPointQ =
            "INSERT OR REPLACE INTO control_point VALUES \
             \ (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        addArea :: ToRow a => a -> [SQLData]
        addArea = ([toField area] ++) . toRow
        cps = controlPoints objects
        transaction = do
            mapM_ (execute conn gateQ . addArea) (gates objects)
            mapM_ (execute conn asteroidQ . addArea) (asteroids objects)
            mapM_ (execute conn controlPointQ . addArea) cps


--external interface

getUser :: UserId -> TagPool -> Process (Maybe User)
getUser uid tagPool = do
    Just pid <- globalWhereIs dbServiceName tagPool
    tag <- getTag tagPool
    Just res <- callTimeout pid (GetUser uid) tag timeoutForCall
    return res


putUser :: User -> Process ()
putUser user = globalNSend dbServiceName $ PutUser user


getAreaObjects :: AreaId -> TagPool -> Process AreaObjects
getAreaObjects aid tagPool = do
    Just pid <- globalWhereIs dbServiceName tagPool
    tag <- getTag tagPool
    Just res <- callTimeout pid (GetAreaObjects aid) tag timeoutForCall
    return res


putAreaObjects :: AreaId -> AreaObjects -> Process ()
putAreaObjects aid objs = globalNSend dbServiceName $ PutAreaObjects aid objs


