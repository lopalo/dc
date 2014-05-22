{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area (areas, startArea, areaProcess, AreaPid(AreaPid),
             enter, clientCmd) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Map as M

import Data.Aeson(ToJSON, Value, Result(Success), fromJSON)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea)
import qualified Connection as C
import Types (UserId, AreaId, AreaPid(AreaPid))

areas = ["start_area"]
startArea = "start_area"
tickMicroseconds = 1000000


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick

type ForwardData = (ProcessId, Connection, String)


type Connections = M.Map UserId Connection
type UserName = String
type Users = M.Map UserId UserName
type UserIds = M.Map Connection UserId

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    connections :: Connections,
                    userIds :: UserIds,
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

handleEnter :: State -> (String, (UserId, String), Connection) -> Process State
handleEnter state ("enter", userInfo, conn) = do
    let (userId, name) = userInfo
        us' = M.insert userId name $ users state
        uids' = M.insert conn userId $ userIds state
        cs' = M.insert userId conn $ connections  state
        state' = state{users=us', connections=cs', userIds=uids'}
    selfPid <- makeSelfPid
    setArea conn selfPid
    broadcastCmd state' "entered" name
    return state'

handleEcho :: State -> (String, String, Connection) -> Process State
handleEcho state ("echo", txt, conn) = do
    sendCmd conn "echo-reply" $ areaId state ++ " echo: " ++ txt
    return state


areaProcess :: AreaId -> Process ()
areaProcess aId = do
    let state = State{areaId=aId,
                      tickNumber=0,
                      connections=M.empty,
                      users=M.empty,
                      userIds=M.empty}
    scheduleTick tickMicroseconds
    loop state
    return ()


loop :: State -> Process State
loop state = receiveWait handlers >>= loop
    where prepare h = match (h state)
          handlers = [prepare handleTick,
                      prepare handleEnter,
                      prepare handleEcho]


sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd conn cmd body = C.sendCmd conn ("area." ++ cmd) body


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd body =
    C.broadcastCmd (M.elems (connections state)) ("area." ++ cmd) body

strKeys :: Users -> M.Map String String
strKeys = M.mapKeys show

makeSelfPid :: Process AreaPid
makeSelfPid = getSelfPid >>= return . AreaPid


forward' :: Serializable a => a -> ForwardData -> Process ()
forward' parsedBody (areaPid, conn, cmd) = send areaPid (cmd, parsedBody, conn)

parseClientCmd :: String -> Value -> ForwardData -> Process ()
parseClientCmd "echo" body = do
    let Success txt = fromJSON body :: Result String
    forward' txt

--external interface

enter :: AreaId -> UserId -> Connection -> String -> Process ()
enter areaId' userId conn name = do
    Just areaPid <- whereis areaId'
    send areaPid ("enter", (userId, name), conn)


clientCmd :: AreaPid -> String -> Value -> Connection -> Process ()
clientCmd (AreaPid pid) cmd body conn =
    parseClientCmd cmd body (pid, conn, cmd)




