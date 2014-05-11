{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area (areas, startArea, areaProcess, AreaPid(AreaPid)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Map as M

import Control.Distributed.Process

import Connection (Connection)
import Types (UserId, AreaId, AreaPid(AreaPid))

areas = ["start_area"]
startArea = "start_area"
tickMicroseconds = 1000000


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


type Connections = M.Map UserId Connection

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    connections :: Connections}


scheduleTick :: Int -> Process ProcessId
scheduleTick microseconds = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO (threadDelay microseconds)
        send selfPid TimeTick

handleTick :: State -> TimeTick -> Process State
handleTick state TimeTick = do
    --TODO: publish tick info
    let tNum = tickNumber state
    say $ "handle tick " ++ show tNum ++ " of area: " ++ areaId state
    let state' = state{tickNumber=tNum + 1}
    scheduleTick tickMicroseconds
    return state'


areaProcess :: AreaId -> Process ()
areaProcess aId = do
    let state = State{areaId=aId,
                      tickNumber=0,
                      connections=M.empty}
    scheduleTick tickMicroseconds
    loop state
    return ()


loop :: State -> Process State
loop state = receiveWait (prepare handlers) >>= loop
    --TODO: enter handler
    where handlers = [handleTick]
          prepare = map (\h -> match (h state))

