
module Area.Area (areaProcess, AreaPid(AreaPid), enter, clientCmd) where

import Control.Monad (liftM)
import qualified Data.Map.Strict as M

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
    let uid = uidByConn conn state
        user = userByConn conn state
        dt = distance (U.pos user) toPos / (fromIntegral (U.speed user) / 1000)
        action = MoveDistance{startTs=now,
                              endTs=now + round dt,
                              from=U.pos user,
                              to=toPos}
        replace MoveDistance{} = True
        replace _ = False
    return $ replaceUserAction uid replace action state


handleIgnite :: State -> (String, Float, Connection) -> Process State
handleIgnite state ("ignite", dmgSpeed, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        action = Burning{previousTs=now,
                         damageSpeed=dmgSpeed}
    return $ addUserAction uid action state


areaProcess :: AreaId -> Process ()
areaProcess aid = do
    now <- liftIO milliseconds
    let state = State{areaId=aid,
                      tickNumber=0,
                      timestamp=now,
                      connections=M.empty,
                      users=M.empty,
                      userIds=M.empty,
                      events=[],
                      eventsForBroadcast=[]}
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
        evalState = receiveWait handlers >>= evaluate
        handle = evalState `catches` logException state


uidByConn :: Connection -> State -> UserId
uidByConn conn state = userIds state M.! conn

userByConn :: Connection -> State -> U.User
userByConn conn state = users state M.! uidByConn conn state

updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f uid state =
    let actions = f $ U.actions user
        us = users state
        user = us M.! uid
        us' = M.insert uid user{U.actions=actions} us
    in state{users=us'}


addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action:) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                                . cancelUserAction uid f


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




