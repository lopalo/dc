{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Area (areaProcess, AreaPid(AreaPid), enter, clientCmd) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.))
import qualified Data.Map.Strict as M

import Data.Aeson(Value, Result(Success), fromJSON, object, (.=))
import Data.Lens.Common ((^%=))
import Control.Distributed.Process hiding (forward)
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea)
import Utils (milliseconds, logException, evaluate)
import qualified Settings as S
import Types (UserId, UserName, AreaId, AreaPid(AreaPid))
import User (AreaUserInfo)
import Area.Types (Pos(..))
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, sendCmd, broadcastCmd)
import Area.State
import Area.Tick (handleTick, scheduleTick)



data Echo = Echo !String deriving (Generic, Typeable)
instance Binary Echo

data Enter = Enter !AreaUserInfo deriving (Generic, Typeable)
instance Binary Enter

data EnterArea = EnterArea !AreaId deriving (Generic, Typeable)
instance Binary EnterArea

data MoveTo = MoveTo !Pos deriving (Generic, Typeable)
instance Binary MoveTo

data Ignite = Ignite !Float deriving (Generic, Typeable)
instance Binary Ignite


handleEnter :: State -> (Enter, Connection) -> Process State
handleEnter state (Enter userInfo, conn) = do
    let (uid, userName) = userInfo
        startPos = S.startAreaPos
        user = U.User{U.userId=uid,
                      U.name=userName,
                      U.pos=uncurry Pos startPos,
                      U.speed=S.areaUserSpeed,
                      U.durability=S.initUserDurability,
                      U.actions=[]}
        state' = (users' ^%= addUser uid conn user) state
    selfPid <- makeSelfPid
    setArea conn selfPid
    sendCmd conn "init" $ initData state'
    broadcastCmd state "entered" $ U.initClientInfo user -- broadcast for all, except a new user
    return state'


handleEcho :: State -> (Echo, Connection) -> Process State
handleEcho state (Echo txt, conn) = do
    sendCmd conn "echo-reply" $ areaId state ++ " echo: " ++ txt
    return state


handleEnterArea :: State -> (EnterArea, Connection) -> Process State
handleEnterArea state (EnterArea aid, conn) = do
    let U.User{U.userId=uid, U.name=name} = conn `userByConn` state
    enter aid (uid, name) conn
    return $ (users' ^%= deleteUser uid) state


handleMoveTo :: State -> (MoveTo, Connection) -> Process State
handleMoveTo state (MoveTo toPos, conn) = do
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


handleIgnite :: State -> (Ignite, Connection) -> Process State
handleIgnite state (Ignite dmgSpeed, conn) = do
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
                      users=us,
                      events=[],
                      eventsForBroadcast=[]}
        us = Users{connections=M.empty, usersData=M.empty, userIds=M.empty}
    scheduleTick S.areaTickMilliseconds
    loop state
    return ()


loop :: State -> Process State
loop state = handle >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers don't work correctly if they have the same type
        handlers = [prepare (flip handleTick),
                    prepare handleEnterArea,
                    prepare handleEnter,
                    prepare handleMoveTo,
                    prepare handleIgnite,
                    prepare handleEcho]
        evalState = receiveWait handlers >>= evaluate
        handle = evalState `catches` logException state


uidByConn :: Connection -> State -> UserId
uidByConn conn state = (userIds . users) state M.! conn

userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f uid = usersData' . users' ^%= M.adjust update uid
    where update user = user{U.actions=f $ U.actions user}

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action:) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f


initData :: State -> Value
initData state =
    let res = object ["areaId" .= areaId state,
                      "objects" .= usersInfo]
        usersInfo = map U.initClientInfo $ M.elems $ usersData $ users state
    in res


makeSelfPid :: Process AreaPid
makeSelfPid = liftM AreaPid getSelfPid


forward :: Serializable a => a -> (ProcessId, Connection) -> Process ()
forward parsed (areaPid, conn) = do
    evaluate parsed
    send areaPid (parsed, conn)

parseClientCmd :: String -> Value -> (ProcessId, Connection) -> Process ()
parseClientCmd "echo" body = do
    let Success txt = fromJSON body :: Result String
    forward (Echo txt)
parseClientCmd "enter_area" body = do
    let Success aid = fromJSON body :: Result AreaId
    forward (EnterArea aid)
parseClientCmd "move_to" body = do
    let Success toPos = fromJSON body :: Result Pos
    forward (MoveTo toPos)
parseClientCmd "ignite" body = do
    let Success dmgSpeed = fromJSON body :: Result Float
    forward (Ignite dmgSpeed)


--external interface

enter :: AreaId -> AreaUserInfo -> Connection -> Process ()
enter aid userInfo conn = do
    Just areaPid <- whereis aid
    send areaPid (Enter userInfo , conn)


clientCmd :: AreaPid -> String -> Value -> Connection -> Process ()
clientCmd (AreaPid pid) cmd body conn =
    parseClientCmd cmd body (pid, conn)




