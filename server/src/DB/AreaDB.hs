{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module DB.AreaDB (
    AreaObjects(..),
    areaDBProcess,
    getAreaObjects, putAreaObjects,
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log)
import Control.Monad (forever, when)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Set as Set

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag, newTagPool)
import Control.Distributed.Process.Extras.Call (
    callResponse, callTimeout, multicall
    )
import Database.LevelDB.Base (
    DB, BatchOp(Put, Del), get, write,
    defaultReadOptions, defaultWriteOptions
    )
import Database.LevelDB.Iterator (withIter)
import Database.LevelDB.Streaming (
    KeyRange(AllKeys), Direction(Asc),
    entrySlice, keySlice
    )
import qualified Data.Stream.Monadic as Stream

import Base.GlobalRegistry (globalWhereIsByPrefix)
import Types (
    Ts, ServiceId, AreaId(..), LogLevel(..),
    ServiceType(AreaDB), prefix
    )
import Utils (safeReceive, timeoutForCall, milliseconds)
import Area.Types (objId)
import Area.Objects.Gate (Gate)
import Area.Objects.Asteroid (Asteroid)
import Area.Objects.ControlPoint (ControlPoint)
import DB.Types (Persistent(fromByteString, toByteString))
import DB.Utils (dbProcess, key, log)
import qualified DB.Settings as DS

data AreaObjects = AreaObjects {
    gates :: ![Gate],
    asteroids :: ![Asteroid],
    controlPoints :: ![ControlPoint]
    }
    deriving (Generic, Typeable)

instance Binary AreaObjects


data GetAreaObjects = GetAreaObjects deriving (Generic, Typeable)

instance Binary GetAreaObjects


data GetUpdateTs = GetUpdateTs deriving (Generic, Typeable)

instance Binary GetUpdateTs


data PutAreaObjects =
    PutAreaObjects AreaObjects
    deriving (Generic, Typeable)

instance Binary PutAreaObjects


updateTsKey :: ByteString
updateTsKey = "update-timestamp"


areaDBProcess :: DS.Settings -> ServiceId -> Process ()
areaDBProcess = dbProcess loop


loop :: DB -> Process ()
loop db = forever $ safeReceive handlers ()
    where
        prepareCall h = callResponse (h db)
        handlers = [
            prepareCall handlePutAreaObjects,
            prepareCall handleGetUpdateTs,
            prepareCall handleGetAreaObjects,
            matchUnknown (return ())
            ]


handleGetAreaObjects :: DB -> GetAreaObjects -> Process (AreaObjects, ())
handleGetAreaObjects db GetAreaObjects = do
    objs <- withIter db defaultReadOptions getObjects
    return (objs, ())
    where
        emptyObjects = AreaObjects [] [] []
        getObjects i =
            Stream.foldl' handleEntry emptyObjects $ entrySlice i AllKeys Asc

        handleEntry objs (k, v)
            | "gate:" `isPrefixOf` k =
                objs{gates=fromByteString v : gates objs}
            | "asteroid:" `isPrefixOf` k =
                objs{asteroids=fromByteString v : asteroids objs}
            | "cp:" `isPrefixOf` k =
                objs{controlPoints=fromByteString v : controlPoints objs}
            | otherwise = objs


handleGetUpdateTs :: DB -> GetUpdateTs -> Process (Ts, ())
handleGetUpdateTs db GetUpdateTs = do
    res <- get db defaultReadOptions updateTsKey
    return (fromMaybe 0 $ read . unpack <$> res, ())


handlePutAreaObjects :: DB -> PutAreaObjects -> Process (Bool, ())
handlePutAreaObjects db (PutAreaObjects objects) = do
    let
        getObjectsKeys i =
            let slice = keySlice i AllKeys Asc
                handleKey k
                    | "gate:" `isPrefixOf` k = True
                    | "asteroid:" `isPrefixOf` k = True
                    | "cp:" `isPrefixOf` k = True
                    | otherwise = False
                slice' = Stream.filter handleKey slice
            in Set.fromList <$> Stream.toList slice'
    objKeys <- withIter db defaultReadOptions getObjectsKeys
    ts <- liftIO milliseconds
    let updateTs = Put updateTsKey $ pack $ show ts
        metaBatch = [updateTs]
        putOp obj = Put (key $ objId obj) (toByteString obj)
        batch =
            concat [
                map putOp $ gates objects,
                map putOp $ asteroids objects,
                map putOp $ controlPoints objects,
                metaBatch
                ]
        objKeys' = Set.fromList $ (\(Put k _) -> k) <$> batch
        delKeys = objKeys Set.\\ objKeys'
        batch' = (Del <$> Set.toList delKeys) ++ batch
    write db defaultWriteOptions batch'
    return (True, ())


getDBForArea :: AreaId -> TagPool -> Process [ProcessId]
getDBForArea (AreaId aid) tagPool =
    globalWhereIsByPrefix pr tagPool
    where pr = prefix AreaDB ++ aid ++ "|"


replicaAmountError :: AreaId -> Process ()
replicaAmountError (AreaId aid) =
    log Error $ "Not enough replicas for area: " ++ aid


--external interface


getAreaObjects :: AreaId -> Int -> TagPool -> Process AreaObjects
getAreaObjects aid minReplicas tagPool = do
    dbPids <- getDBForArea aid tagPool
    tag <- getTag tagPool
    tsList <- multicall dbPids GetUpdateTs tag timeoutForCall
    let dbTsList = zip tsList dbPids :: [(Maybe Ts, ProcessId)]
        dbTsList' = [(ts, p) | (Just ts, p) <- dbTsList]
        (_, pid) = maximum dbTsList'
        failure = length dbTsList' < minReplicas
    when failure $ do
        replicaAmountError aid
        terminate
    tag' <- getTag tagPool
    Just res <- callTimeout pid GetAreaObjects tag' timeoutForCall
    return res


putAreaObjects :: AreaId -> Int -> AreaObjects -> Process ()
putAreaObjects aid minReplicas objs = do
    areaPid <- getSelfPid
    spawnLocal $ do
        tagPool <- newTagPool
        dbPids <- getDBForArea aid tagPool
        tag <- getTag tagPool
        responses <- multicall dbPids (PutAreaObjects objs) tag timeoutForCall
        let failure = length [True | Just True <- responses] < minReplicas
        when failure $ do
            exit areaPid ("No db replicas" :: String)
            replicaAmountError aid
    return ()


