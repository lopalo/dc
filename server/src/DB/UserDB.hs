{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB.UserDB (userDBProcess, putUser, getUser) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack, unpack)
import Data.String.Utils (split, startswith)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag)
import Control.Distributed.Process.Extras.Call (
    callResponse, callTimeout, multicall
    )
import Database.LevelDB.Base (
    DB, BatchOp(Put), get, write,
    defaultReadOptions, defaultWriteOptions
    )
import Data.Digest.CRC32 (crc32)

import Base.GlobalRegistry (getNameList)
import Types (
    UserId(..), ServiceId, ServiceType(UserDB),
    LogLevel(Error), Ts, prefix, delIdPrefix
    )
import Utils (safeReceive, timeoutForCall, milliseconds)
import User.Types (User, userId)
import DB.Utils (dbProcess, key, log)
import DB.Types (Persistent(fromByteString, toByteString))
import qualified DB.Settings as DS


type ShardNumber = Int


type ShardAmount = Int


type ShardInfo = (ShardNumber, ShardAmount)


data GetUpdateTs = GetUpdateTs UserId deriving (Generic, Typeable)

instance Binary GetUpdateTs


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
        prepareCall h = callResponse (h db)
        handlers = [
            prepareCall handlePutUser,
            prepareCall handleGetUpdateTs,
            prepareCall handleGetUser,
            matchUnknown (return ())
            ]


handleGetUser :: DB -> GetUser -> Process (Maybe User, ())
handleGetUser db (GetUser uid) = do
    res <- get db defaultReadOptions $ key uid
    return (res >>= Just . fromByteString, ())


handleGetUpdateTs :: DB -> GetUpdateTs -> Process (Ts, ())
handleGetUpdateTs db (GetUpdateTs (UserId ident)) = do
    res <- get db defaultReadOptions updateTsKey
    return (fromMaybe 0 $ read . unpack <$> res, ())
    where updateTsKey = key $ updateTsKeyPrefix ++ ident


handlePutUser :: DB -> PutUser -> Process (Bool, ())
handlePutUser db (PutUser user) = do
    ts <- liftIO milliseconds
    let uid = userId user
        UserId ident = uid
        batch = [
            Put (key uid) (toByteString user),
            Put (key $ updateTsKeyPrefix ++ ident) (pack $ show ts)
            ]
    write db defaultWriteOptions batch
    return (True, ())


getDBForUser :: UserId -> TagPool -> Process [ProcessId]
getDBForUser (UserId uid) tagPool = do
    let namePair (name, pid, _) = (name, pid)
    res <- fmap (map namePair) (getNameList (prefix UserDB) tagPool)
    case res of
        [] -> do
            log Error $ "There are no user databases"
            terminate
        nameList ->
            let name = fst $ head nameList
                (_, amount) = getShardInfo name
                hash = fromIntegral $ crc32 $ key uid
                number = hash `rem` amount
                shardPrefix = shardName (number, amount) ++ "|"
                f = startswith shardPrefix . fst
                dbPids = map snd $ filter f nameList
            in return dbPids


getShardInfo :: String -> ShardInfo
getShardInfo name =
    let dbId = delIdPrefix (init $ prefix UserDB) name
        [shardInfo, _] = split "|" dbId;
        [number, amount] = split "-" $ tail $ init shardInfo;
    in (read number, read amount)


shardName :: ShardInfo -> String
shardName (number, amount) =
    prefix UserDB ++ "[" ++ show number ++ "-" ++ show amount ++ "]"


replicaAmountError :: UserId -> Process ()
replicaAmountError (UserId uid) =
    log Error $ "Not enough replicas for user: " ++ uid


--external interface


getUser :: UserId -> Int -> TagPool -> Process (Maybe User)
getUser uid minReplicas tagPool = do
    dbPids <- getDBForUser uid tagPool
    tag <- getTag tagPool
    tsList <- multicall dbPids (GetUpdateTs uid) tag timeoutForCall
    let dbTsList = zip tsList dbPids :: [(Maybe Ts, ProcessId)]
        dbTsList' = [(ts, p) | (Just ts, p) <- dbTsList]
        (_, pid) = maximum dbTsList'
        failure = length dbTsList' < minReplicas
    when failure $ do
        replicaAmountError uid
        terminate
    tag' <- getTag tagPool
    Just res <- callTimeout pid (GetUser uid) tag' timeoutForCall
    return res


putUser :: User -> Int -> TagPool -> Process ()
putUser user minReplicas tagPool = do
    userPid <- getSelfPid
    spawnLocal $ do
        dbPids <- getDBForUser (userId user) tagPool
        tag <- getTag tagPool
        responses <- multicall dbPids (PutUser user) tag timeoutForCall
        let failure = length [True | Just True <- responses] < minReplicas
        when failure $ do
            exit userPid ("No db replicas" :: String)
            replicaAmountError $ userId user
    return ()

