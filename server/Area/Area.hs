{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.Area (areaProcess, AreaPid(AreaPid), enter, clientCmd) where

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
import Utils (milliseconds, logDebug)
import qualified Connection as C
import qualified Settings as S
import Types (UserId, AreaId, AreaPid(AreaPid))
import Area.Types (UserName, Pos(..))
import Area.User (User(..), tickClientInfo)
import Area.Action (Action(..))



data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick

type ForwardData = (ProcessId, Connection, String)


type Connections = M.Map UserId Connection
type Users = M.Map UserId User
type UserIds = M.Map Connection UserId

data State = State {areaId :: AreaId,
                    tickNumber :: Int,
                    connections :: Connections,
                    userIds :: UserIds,
                    users :: Users} --TODO: add settings


scheduleTick :: Int -> Process ProcessId
scheduleTick milliseconds' = do
    selfPid <- getSelfPid
    spawnLocal $ do
        liftIO $ threadDelay $ milliseconds' * 1000
        send selfPid TimeTick

handleTick :: State -> TimeTick -> Process State
handleTick state TimeTick = do
    --TODO: use StateT
    let tNum = tickNumber state
    logDebug $ "handle tick " ++ show tNum ++ " of area: " ++ areaId state
    let state' = state{tickNumber=tNum + 1}
    scheduleTick S.areaTickMilliseconds
    broadcastCmd state' "tick" $ tickClientData state
    return state'

handleEnter :: State ->
               (String, (UserId, UserName), Connection) ->
               Process State
handleEnter state ("enter", userInfo, conn) = do
    let (uid, userName) = userInfo
        startPos = S.startAreaPos
        user = User{userId=uid,
                    name=userName,
                    pos=Pos (fst startPos) (snd startPos),
                    speed=S.areaUserSpeed,
                    actions=[]}
        us' = M.insert uid user $ users state
        uids' = M.insert conn uid $ userIds state
        cs' = M.insert uid conn $ connections  state
        state' = state{users=us', connections=cs', userIds=uids'}
    selfPid <- makeSelfPid
    setArea conn selfPid
    broadcastCmd state' "entered" userName
    return state'

handleEcho :: State -> (String, String, Connection) -> Process State
handleEcho state ("echo", txt, conn) = do
    sendCmd conn "echo-reply" $ areaId state ++ " echo: " ++ txt
    return state


handleMoveTo :: State -> (String, Pos, Connection) -> Process State
handleMoveTo state ("move_to", toPos, conn) = do
    now <- liftIO milliseconds
    let users' = users state
        uid = userByConn conn state
        user = users' M.! uid
        actions' = action:actions user
        action = MoveDistance{startTs=now,
                              endTs=now + 4000, --TODO: get real duration
                              from=pos user,
                              to=toPos}
        users'' = M.insert uid user{actions=actions'} $ users'
    return state{users=users''}



areaProcess :: AreaId -> Process ()
areaProcess aId = do
    let state = State{areaId=aId,
                      tickNumber=0,
                      connections=M.empty,
                      users=M.empty,
                      userIds=M.empty}
    scheduleTick S.areaTickMilliseconds
    loop state
    return ()


loop :: State -> Process State
loop state = receiveWait handlers >>= loop
    --TODO: catch exceptions from handlers and log
    where prepare h = match (h state)
          handlers = [prepare handleTick,
                      prepare handleEnter,
                      prepare handleMoveTo,
                      prepare handleEcho]


sendCmd :: ToJSON a => Connection -> String -> a -> Process ()
sendCmd conn cmd body = C.sendCmd conn ("area." ++ cmd) body


broadcastCmd :: ToJSON a => State -> String -> a -> Process ()
broadcastCmd state cmd body =
    C.broadcastCmd (M.elems (connections state)) ("area." ++ cmd) body


tickClientData :: State -> [Value]
tickClientData = map tickClientInfo . M.elems . users


userByConn :: Connection -> State -> UserId
userByConn conn state = userIds state M.! conn


makeSelfPid :: Process AreaPid
makeSelfPid = getSelfPid >>= return . AreaPid


forward' :: Serializable a => a -> ForwardData -> Process ()
forward' parsedBody (areaPid, conn, cmd) = send areaPid (cmd, parsedBody, conn)

parseClientCmd :: String -> Value -> ForwardData -> Process ()
parseClientCmd "echo" body = do
    let Success txt = fromJSON body :: Result String
    forward' txt
parseClientCmd "move_to" body = do
    let Success toPos = fromJSON body :: Result Pos
    forward' toPos

--external interface

enter :: AreaId -> UserId -> Connection -> String -> Process ()
enter areaId' userId' conn name' = do
    Just areaPid <- whereis areaId'
    send areaPid ("enter", (userId', name'), conn)


clientCmd :: AreaPid -> String -> Value -> Connection -> Process ()
clientCmd (AreaPid pid) cmd body conn =
    parseClientCmd cmd body (pid, conn, cmd)




