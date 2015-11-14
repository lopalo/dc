{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB (dbProcess, putUser, getUser) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (callResponse, callAt)
import Database.SQLite.Simple (Only(..), Connection, open,
                               close, query, execute)

import GlobalRegistry (globalRegister, globalWhereIs, globalNSend)
import Types (UserId(..))
import Utils (safeReceive)
import User.Types (User)
import qualified Settings as S


data GetUser = GetUser UserId deriving (Generic, Typeable)
instance Binary GetUser


data PutUser = PutUser User deriving (Generic, Typeable)
instance Binary PutUser

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
                    prepareCall handleGetUser]

handleGetUser :: Connection -> GetUser -> Process (Maybe User, ())
handleGetUser conn (GetUser (UserId uid)) = do
    res <- liftIO $ query conn "select * from user where id = ?" (Only uid)
    case res of
        [user] -> return (Just user, ())
        [] -> return (Nothing, ())


handlePutUser :: Connection -> PutUser -> Process ()
handlePutUser conn (PutUser user) = do
    liftIO $ execute conn str user
    return ()
    where
        str = "insert or replace into user values (?, ?, ?, ?, ?)"


--external interface

getUser :: UserId -> TagPool -> Process (Maybe User)
getUser uid tagPool = do
    Just pid <- globalWhereIs dbServiceName
    tag <- getTag tagPool
    res <- callAt pid (GetUser uid) tag
    return $ fromMaybe Nothing res

putUser :: User -> Process ()
putUser user = globalNSend dbServiceName $ PutUser user
