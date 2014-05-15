{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area (areas, startArea, areaProcess, AreaPid(AreaPid),
             enter) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Map as M

import Data.Aeson(ToJSON)
import Data.Aeson.Types
import Control.Distributed.Process

import Connection (Connection)
import qualified Connection as C
import Types (UserId, AreaId, AreaPid(AreaPid))

areas = ["start_area"]
startArea = "start_area"
tickMicroseconds = 1000000


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


type Connections = M.Map UserId Connection
type UserName = String
type Users = M.Map UserId UserName

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    connections :: Connections,
                    users :: Users}


scheduleTick :: Int -> Process ProcessId
scheduleTick microseconds = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO (threadDelay microseconds)
        send selfPid TimeTick

handleTick :: State -> TimeTick -> Process State
handleTick state TimeTick = do
    let tNum = tickNumber state
    say $ "handle tick " ++ show tNum ++ " of area: " ++ areaId state
    let state' = state{tickNumber=tNum + 1}
    scheduleTick tickMicroseconds
    broadcastCmd state' "tick" $ strKeys $ users state
    return state'

handleEnter :: State -> (String, (UserId, Connection, String)) -> Process State
handleEnter state@(State{users=us, connections=cs}) ("enter", userInfo) = do
    let (userId, conn, name) = userInfo
        us' = M.insert userId name us
        cs' = M.insert userId conn cs
        state' = state{users=us', connections=cs'}
    broadcastCmd state' "entered" name
    return state'


areaProcess :: AreaId -> Process ()
areaProcess aId = do
    let state = State{areaId=aId,
                      tickNumber=0,
                      connections=M.empty,
                      users=M.empty}
    scheduleTick tickMicroseconds
    loop state
    return ()


loop :: State -> Process State
loop state = receiveWait handlers >>= loop
    where prepare h = match (h state)
          handlers = [prepare handleTick, prepare handleEnter]


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd body =
    C.broadcastCmd (M.elems (connections state)) ("area." ++ cmd) body

strKeys :: Users -> M.Map String String
strKeys = M.mapKeys show

--external interface

enter :: AreaId -> UserId -> Connection -> String -> Process ()
enter aId userId conn name = do
    Just areaPid <- whereis aId
    send areaPid ("enter", (userId, conn, name))

