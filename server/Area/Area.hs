{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Area (areaProcess, AreaPid(AreaPid), enter, clientCmd) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Map as M

import Data.Aeson(ToJSON, Value, Result(Success), fromJSON)
import Control.Distributed.Process hiding (forward)
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea)
import Utils (milliseconds, logDebug, logException, evaluate)
import qualified Connection as C
import qualified Settings as S
import Types (UserId, AreaId, AreaPid(AreaPid))
import Area.Types (UserName, Pos(..))
import qualified Area.User as U
import Area.Action (Action(..))



data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick

type ForwardData = (ProcessId, Connection, String)


type Connections = M.Map UserId Connection
type Users = M.Map UserId U.User
type UserIds = M.Map Connection UserId

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    connections :: Connections,
                    userIds :: UserIds,
                    users :: Users} --TODO: add settings


scheduleTick :: Int -> Process ProcessId
scheduleTick ms = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO $ threadDelay $ ms * 1000
        send selfPid TimeTick

handleTick :: State -> TimeTick -> Process State
handleTick state TimeTick = do
    --TODO: use StateT
    let tNum = tickNumber state
    logDebug $ "handle tick " ++ show tNum ++ " of area: " ++ areaId state
    let state' = state{tickNumber=tNum + 1}
    scheduleTick S.areaTickMilliseconds
    broadcastCmd state' "tick" $ tickClientData state'
    return state'

handleEnter :: State ->
               (String, (UserId, UserName), Connection) ->
               Process State
handleEnter state ("enter", userInfo, conn) = do
    let (uid, userName) = userInfo
        startPos = S.startAreaPos
        user = U.User{U.userId=uid,
                      U.name=userName,
                      U.pos=Pos (fst startPos) (snd startPos),
                      U.speed=S.areaUserSpeed,
                      U.actions=[]}
        us = M.insert uid user $ users state
        uids = M.insert conn uid $ userIds state
        cs = M.insert uid conn $ connections  state
        state' = state{users=us, connections=cs, userIds=uids}
    selfPid <- makeSelfPid
    setArea conn selfPid
    broadcastCmd state' "entered" userName --TODO: client info
    return state'

handleEcho :: State -> (String, String, Connection) -> Process State
handleEcho state ("echo", txt, conn) = do
    sendCmd conn "echo-reply" $ areaId state ++ " echo: " ++ txt
    return state


handleMoveTo :: State -> (String, Pos, Connection) -> Process State
handleMoveTo state ("move_to", toPos, conn) = do
    now <- liftIO milliseconds
    let us = users state
        uid = userByConn conn state
        user = us M.! uid
        actions = action:U.actions user
        action = MoveDistance{startTs=now,
                              endTs=now + 4000, --TODO: get real duration
                              from=U.pos user,
                              to=toPos}
        us' = M.insert uid user{U.actions=actions} $ us
    return state{users=us'}



areaProcess :: AreaId -> Process ()
areaProcess aid = do
    let state = State{areaId=aid,
                      tickNumber=0,
                      connections=M.empty,
                      users=M.empty,
                      userIds=M.empty}
    scheduleTick S.areaTickMilliseconds
    loop state
    return ()


loop :: State -> Process State
loop state = handle >>= loop
    where
        prepare h = match (h state)
        handlers = [prepare handleTick,
                    prepare handleEnter,
                    prepare handleMoveTo,
                    prepare handleEcho]
        handle = receiveWait handlers `catches` logException state


sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd conn cmd = C.sendCmd conn ("area." ++ cmd)


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd =
    C.broadcastCmd (M.elems (connections state)) ("area." ++ cmd)


tickClientData :: State -> [Value]
tickClientData = map U.tickClientInfo . M.elems . users


userByConn :: Connection -> State -> UserId
userByConn conn state = userIds state M.! conn


makeSelfPid :: Process AreaPid
makeSelfPid = getSelfPid >>= return . AreaPid


forward :: Serializable a => a -> ForwardData -> Process ()
forward parsedBody (areaPid, conn, cmd) = do
    evaluate parsedBody
    send areaPid (cmd, parsedBody, conn)

parseClientCmd :: String -> Value -> ForwardData -> Process ()
parseClientCmd "echo" body = do
    let Success txt = fromJSON body :: Result String
    forward txt
parseClientCmd "move_to" body = do
    let Success toPos = fromJSON body :: Result Pos
    forward toPos

--external interface

enter :: AreaId -> UserId -> Connection -> String -> Process ()
enter aid uid conn userName = do
    Just areaPid <- whereis aid
    send areaPid ("enter", (uid, userName), conn)


clientCmd :: AreaPid -> String -> Value -> Connection -> Process ()
clientCmd (AreaPid pid) cmd body conn =
    parseClientCmd cmd body (pid, conn, cmd)




