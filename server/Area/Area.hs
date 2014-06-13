
module Area.Area (areaProcess, AreaPid(AreaPid), enter, clientCmd) where

import Control.Monad (liftM)
import qualified Data.Map as M

import Data.Aeson(Value, Result(Success), fromJSON)
import Control.Distributed.Process hiding (forward)
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea)
import Utils (milliseconds, logException, evaluate)
import qualified Settings as S
import Types (UserId, AreaId, AreaPid(AreaPid))
import Area.Types (UserName, Pos(..))
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, sendCmd, broadcastCmd)
import Area.State
import Area.Tick (handleTick, scheduleTick)



type ForwardData = (ProcessId, Connection, String)


handleEnter :: State ->
               (String, (UserId, UserName), Connection) ->
               Process State
handleEnter state ("enter", userInfo, conn) = do
    let (uid, userName) = userInfo
        startPos = S.startAreaPos
        user = U.User{U.userId=uid,
                      U.name=userName,
                      U.pos= uncurry Pos startPos,
                      U.speed=S.areaUserSpeed,
                      U.durability=S.initUserDurability,
                      U.actions=[]}
        us = M.insert uid user $ users state
        uids = M.insert conn uid $ userIds state
        cs = M.insert uid conn $ connections  state
        state' = state{users=us, connections=cs, userIds=uids}
    selfPid <- makeSelfPid
    setArea conn selfPid
    broadcastCmd state' "entered" $ U.initClientInfo user
    return state'


handleEcho :: State -> (String, String, Connection) -> Process State
handleEcho state ("echo", txt, conn) = do
    sendCmd conn "echo-reply" $ areaId state ++ " echo: " ++ txt
    return state


handleMoveTo :: State -> (String, Pos, Connection) -> Process State
handleMoveTo state ("move_to", toPos, conn) = do
    now <- liftIO milliseconds
    let user = userByConn conn state
        dt = distance (U.pos user) toPos / (fromIntegral (U.speed user) / 1000)
        action = MoveDistance{startTs=now,
                              endTs=now + round dt,
                              from=U.pos user,
                              to=toPos}
        replace MoveDistance{} = True
        replace _ = False
    --TODO: catch key error inside error handler
    return $ replaceUserAction user replace action state


handleIgnite :: State -> (String, Float, Connection) -> Process State
handleIgnite state ("ignite", dmgSpeed, conn) = do
    now <- liftIO milliseconds
    let user = userByConn conn state
        action = Burning{previousTs=now,
                         damageSpeed=dmgSpeed}
    return $ addUserAction user action state


areaProcess :: AreaId -> Process ()
areaProcess aid = do
    now <- liftIO milliseconds
    let state = State{areaId=aid,
                      tickNumber=0,
                      timestamp=now,
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
        handlers = [prepare (flip handleTick),
                    prepare handleEnter,
                    prepare handleMoveTo,
                    prepare handleIgnite,
                    prepare handleEcho]
        handle = receiveWait handlers `catches` logException state


uidByConn :: Connection -> State -> UserId
uidByConn conn state = userIds state M.! conn

userByConn :: Connection -> State -> U.User
userByConn conn state = users state M.! uidByConn conn state

--TODO: use UserId instead U.User
updateUserActions :: ([Action] -> [Action]) -> U.User -> State -> State
updateUserActions f user state =
    let actions = f $ U.actions user
        us = M.insert (U.userId user) user{U.actions=actions} (users state)
    in state{users=us}


addUserAction :: U.User -> Action -> State -> State
addUserAction user action = updateUserActions (action:) user

cancelUserAction :: U.User -> (Action -> Bool) -> State -> State
cancelUserAction user f = updateUserActions (filter (not . f)) user

replaceUserAction :: U.User -> (Action -> Bool) -> Action -> State -> State
replaceUserAction user f action = addUserAction user action --TODO: use updated user
                                . cancelUserAction user f


makeSelfPid :: Process AreaPid
makeSelfPid = liftM AreaPid getSelfPid


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
parseClientCmd "ignite" body = do
    let Success dmgSpeed = fromJSON body :: Result Float
    forward dmgSpeed


--external interface

enter :: AreaId -> UserId -> Connection -> String -> Process ()
enter aid uid conn userName = do
    Just areaPid <- whereis aid
    send areaPid ("enter", (uid, userName), conn)


clientCmd :: AreaPid -> String -> Value -> Connection -> Process ()
clientCmd (AreaPid pid) cmd body conn =
    parseClientCmd cmd body (pid, conn, cmd)




