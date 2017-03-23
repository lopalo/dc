{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB.DB (dbProcess, putUser, getUser) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.UTF8 (fromString)
import Data.String.Utils (split, startswith)

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
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
    UserId(..), ServiceId, ServiceType(DB),
    LogLevel(Error), Ts, prefix, delIdPrefix
    )
import Utils (safeReceive, timeoutForCall, milliseconds)
import User.Types (User, userId)
import DB.Utils (baseDbProcess, log)
import DB.Types (Persistent(fromByteString, toByteString))
import qualified DB.Settings as DS


type ShardNumber = Int


type ShardAmount = Int


type ShardInfo = (ShardNumber, ShardAmount)


data GetUpdateTs = GetUpdateTs String deriving (Generic, Typeable)

instance Binary GetUpdateTs


data GetUser = GetUser UserId deriving (Generic, Typeable)

instance Binary GetUser


data PutUser = PutUser User deriving (Generic, Typeable)

instance Binary PutUser


updateTsKeyPrefix :: String
updateTsKeyPrefix = "update-timestamp:"


dbProcess :: DS.Settings -> ServiceId -> Process ()
dbProcess settings ident = baseDbProcess loop settings $ "shard" ++ ident


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
    res <- get db defaultReadOptions $ fromString $ show uid
    return (res >>= Just . fromByteString, ())


handleGetUpdateTs :: DB -> GetUpdateTs -> Process (Ts, ())
handleGetUpdateTs db (GetUpdateTs key) = do
    res <- get db defaultReadOptions updateTsKey
    return (fromMaybe 0 $ read . unpack <$> res, ())
    where updateTsKey = fromString $ updateTsKeyPrefix ++ key


handlePutUser :: DB -> PutUser -> Process (Bool, ())
handlePutUser db (PutUser user) = do
    ts <- liftIO milliseconds
    let key = show $ userId user
        batch = [
            Put (fromString key) (toByteString user),
            Put (fromString $ updateTsKeyPrefix ++ key) (pack $ show ts)
            ]
    write db defaultWriteOptions batch
    return (True, ())


getShardForKey :: String -> TagPool -> Process [ProcessId]
getShardForKey key tagPool = do
    let namePair (name, pid, _) = (name, pid)
    res <- fmap (map namePair) (getNameList (prefix DB) tagPool)
    case res of
        [] -> do
            log Error $ "There are no databases"
            return []
        nameList ->
            let name = fst $ head nameList
                (_, amount) = getShardInfo name
                hash = fromIntegral $ crc32 $ fromString key
                number = hash `rem` amount
                shardPrefix = shardName (number, amount) ++ "|"
                f = startswith shardPrefix . fst
                dbPids = map snd $ filter f nameList
            in return dbPids


getShardInfo :: String -> ShardInfo
getShardInfo name =
    let dbId = delIdPrefix (init $ prefix DB) name
        [shardInfo, _] = split "|" dbId;
        [number, amount] = split "-" $ tail $ init shardInfo;
    in (read number, read amount)


shardName :: ShardInfo -> String
shardName (number, amount) =
    prefix DB ++ "[" ++ show number ++ "-" ++ show amount ++ "]"


getRecentReplica :: String -> Int -> TagPool -> Process ProcessId
getRecentReplica key minReplicas tagPool = do
    dbPids <- getShardForKey key tagPool
    tag <- getTag tagPool
    tsList <- multicall dbPids (GetUpdateTs key) tag timeoutForCall
    let dbTsList = zip tsList dbPids :: [(Maybe Ts, ProcessId)]
        dbTsList' = [(ts, p) | (Just ts, p) <- dbTsList]
        (_, pid) = maximum dbTsList'
        failure = length dbTsList' < minReplicas
    when failure $ do
        replicaAmountError key
        terminate
    return pid

replicaAmountError :: String -> Process ()
replicaAmountError key =
    log Error $ "Not enough replicas for key: " ++ key


getRecent :: (Serializable a, Serializable b) =>
    a -> String -> Int -> TagPool -> Process b
getRecent msg key minReplicas tagPool = do
    pid <- getRecentReplica key minReplicas tagPool
    tag <- getTag tagPool
    Just res <- callTimeout pid msg tag timeoutForCall
    return res


put :: Serializable a => a -> String -> Int -> TagPool -> Process ()
put msg key minReplicas tagPool = do
    userPid <- getSelfPid
    spawnLocal $ do
        dbPids <- getShardForKey key tagPool
        tag <- getTag tagPool
        responses <- multicall dbPids msg tag timeoutForCall
        let failure = length [True | Just True <- responses] < minReplicas
        when failure $ do
            exit userPid ("db:not-enough-replicas" :: String)
            replicaAmountError key
    return ()


--external interface


getUser :: UserId -> Int -> TagPool -> Process (Maybe User)
getUser uid = getRecent (GetUser uid) (show uid)


putUser :: User -> Int -> TagPool -> Process ()
putUser user minReplicas tagPool = do
    let key = show $ userId user
        msg = PutUser user
    put msg key minReplicas tagPool
