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
import Control.Monad (forever)
import Data.ByteString (isPrefixOf)

import Control.Distributed.Process
import Control.Distributed.Process.Extras (TagPool, getTag, newTagPool)
import Control.Distributed.Process.Extras.Call (callResponse, callTimeout)
import Database.LevelDB.Base (
    DB, BatchOp(Put), write,
    defaultReadOptions, defaultWriteOptions
    )
import Database.LevelDB.Iterator (createIter, releaseIter)
import Database.LevelDB.Streaming (
    KeyRange(AllKeys), Direction(Asc), entrySlice
    )
import qualified Data.Stream.Monadic as Stream

import Base.GlobalRegistry (globalWhereIs)
import Base.Logger (log)
import Types (ServiceId, AreaId(..), LogLevel(..), ServiceType(AreaDB), prefix)
import Utils (safeReceive, timeoutForCall)
import Area.Types (objId)
import Area.Objects.Gate (Gate)
import Area.Objects.Asteroid (Asteroid)
import Area.Objects.ControlPoint (ControlPoint)
import DB.Types (Persistent(fromByteString, toByteString))
import DB.Utils (dbProcess, key)
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


data PutAreaObjects =
    PutAreaObjects AreaObjects
    deriving (Generic, Typeable)

instance Binary PutAreaObjects




areaDBProcess :: DS.Settings -> ServiceId -> Process ()
areaDBProcess = dbProcess loop


loop :: DB -> Process ()
loop db = forever $ safeReceive handlers ()
    where
        prepare h = match (h db)
        prepareCall h = callResponse (h db)
        handlers = [
            prepare handlePutAreaObjects,
            prepareCall handleGetAreaObjects,
            matchUnknown (return ())
            ]


handleGetAreaObjects :: DB -> GetAreaObjects -> Process (AreaObjects, ())
handleGetAreaObjects db GetAreaObjects = do
    objs <- bracket makeIter releaseIter handleIter
    return (objs, ())
    where
        emptyObjects = AreaObjects [] [] []
        makeIter = createIter db defaultReadOptions
        handleIter i =
            Stream.foldl' handleEntry emptyObjects $ entrySlice i AllKeys Asc

        handleEntry objs (k, v)
            | "gate:" `isPrefixOf` k =
                objs{gates=fromByteString v : gates objs}
            | "asteroid:" `isPrefixOf` k =
                objs{asteroids=fromByteString v : asteroids objs}
            | "cp:" `isPrefixOf` k =
                objs{controlPoints=fromByteString v : controlPoints objs}
            | otherwise = objs


handlePutAreaObjects :: DB -> PutAreaObjects -> Process ()
handlePutAreaObjects db (PutAreaObjects objects) = do
    write db defaultWriteOptions $ concat batch
    where
        putOp obj = Put (key $ objId obj) (toByteString obj)
        batch = [
            map putOp $ gates objects,
            map putOp $ asteroids objects,
            map putOp $ controlPoints objects
            ]


getDBForArea :: AreaId -> TagPool -> Process ProcessId
getDBForArea (AreaId aid) tagPool = do
    let name = prefix AreaDB ++ aid ++ "|a"
    res <- globalWhereIs name tagPool
    case res of
        Just dbPid -> return dbPid
        Nothing -> do
            log Error $ "There is no db for area " ++ aid
            terminate


--external interface


getAreaObjects :: AreaId -> TagPool -> Process AreaObjects
getAreaObjects aid tagPool = do
    pid <- getDBForArea aid tagPool
    tag <- getTag tagPool
    Just res <- callTimeout pid GetAreaObjects tag timeoutForCall
    return res


putAreaObjects :: AreaId -> AreaObjects -> Process ()
putAreaObjects aid objs = do
    spawnLocal $ do
        pid <- getDBForArea aid =<< newTagPool
        send pid $ PutAreaObjects objs
    return ()


