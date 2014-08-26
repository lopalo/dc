{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Area (areaProcess, AreaPid(AreaPid), enter, clientCmd) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding ((.))
import Control.Monad (liftM)
import Control.Category ((.))
import qualified Data.Map.Strict as M

import Data.Aeson(ToJSON, Value, Result(Success), fromJSON, object, (.=))
import Data.String.Utils (startswith)
import Data.Lens.Common ((^%=))
import Control.Distributed.Process hiding (forward)
import Control.Distributed.Process.Serializable (Serializable)

import Connection (Connection, setArea, sendResponse)
import Utils (milliseconds, logException, evaluate)
import qualified Settings as S
import Types (UserId, AreaId, AreaPid(AreaPid), RequestNumber)
import User (AreaUserInfo)
import Area.Types (Pos(..))
import qualified Area.User as U
import Area.Action (Action(..))
import Area.Utils (distance, angle, sendCmd)
import Area.State
import Area.Tick (handleTick, scheduleTick)


type ForwardData = (ProcessId, Connection, RequestNumber)

type Response a = (a, State)


data Echo = Echo !String deriving (Generic, Typeable)
instance Binary Echo

data Enter = Enter !AreaUserInfo deriving (Generic, Typeable)
instance Binary Enter

data GetObjectsInfo = GetObjectsInfo ![String] deriving (Generic, Typeable)
instance Binary GetObjectsInfo

data EnterArea = EnterArea !AreaId deriving (Generic, Typeable)
instance Binary EnterArea

data MoveTo = MoveTo !Pos deriving (Generic, Typeable)
instance Binary MoveTo

data Ignite = Ignite !Float deriving (Generic, Typeable)
instance Binary Ignite


handleEnter :: State -> (Enter, Connection) -> Process State
handleEnter state (Enter userInfo, conn) = do
    let (uid, userName) = userInfo
        startPos = (S.startAreaPos . settings) state
        user = U.User{U.userId=uid,
                      U.name=userName,
                      U.pos=uncurry Pos startPos,
                      U.angle=0,
                      U.speed=(S.areaUserSpeed . settings) state,
                      U.durability=(S.initUserDurability . settings) state,
                      U.actions=[]}
        state' = (users' ^%= addUser uid conn user) state
    selfPid <- makeSelfPid
    setArea conn selfPid
    now <- liftIO milliseconds
    sendCmd conn "init" $ object ["areaId" .= areaId state',
                                  "timestamp" .= now]
    return state'


handleEcho :: State -> (Echo, Connection) -> Process (Response String)
handleEcho state (Echo txt, _) = do
    let resp = areaId state ++ " echo: " ++ txt
    return (resp, state)


handleEnterArea :: State -> (EnterArea, Connection) -> Process State
handleEnterArea state (EnterArea aid, conn) = do
    let U.User{U.userId=uid, U.name=name} = conn `userByConn` state
    enter aid (uid, name) conn
    return $ (users' ^%= deleteUser uid) state


handleObjectsInfo :: State -> (GetObjectsInfo, Connection)
                     -> Process (Response [Value])
handleObjectsInfo state (GetObjectsInfo ids, _) = do
    let objects = foldl getObj [] ids
        us = usersData $ users state
        getObj :: [Value] -> String -> [Value]
        getObj res ident
            | "user_id:" `startswith` ident =
                let ident' = read ident :: UserId
                in case ident' `M.lookup` us of
                    Nothing -> res
                    Just user -> U.initClientInfo user:res
    return (objects, state)



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
        updActions = replaceUserAction uid replace action
        updUser = updateUser (\u -> u{U.angle=angle (U.pos u) toPos}) uid
    return $ updActions $ updUser state


handleIgnite :: State -> (Ignite, Connection) -> Process State
handleIgnite state (Ignite dmgSpeed, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        action = Burning{previousTs=now,
                         damageSpeed=dmgSpeed}
    return $ addUserAction uid action state


areaProcess :: S.Settings -> AreaId -> Process ()
areaProcess settings aid = do
    let state = State{areaId=aid,
                      settings=settings,
                      tickNumber=0,
                      users=us,
                      events=[],
                      eventsForBroadcast=[]}
        us = Users{connections=M.empty, usersData=M.empty, userIds=M.empty}
    scheduleTick $ S.areaTickMilliseconds settings
    loop state
    return ()


loop :: State -> Process State
loop state = handle >>= loop
    where
        prepare h = match (h state)
        --NOTE: handlers don't work correctly if they have the same type
        handlers = [prepare handleTick,
                    prepare handleEnterArea,
                    prepare handleEnter,
                    prepare (request handleObjectsInfo),
                    prepare handleMoveTo,
                    prepare handleIgnite,
                    prepare (request handleEcho)]
        evalState = receiveWait handlers >>= evaluate
        handle = evalState `catches` logException state


uidByConn :: Connection -> State -> UserId
uidByConn conn state = (userIds . users) state M.! conn

userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f = updateUser update
    where update user = user{U.actions=f $ U.actions user}

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action:) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f

makeSelfPid :: Process AreaPid
makeSelfPid = liftM AreaPid getSelfPid

request :: (Serializable a, ToJSON b) =>
           (State -> (a, Connection) -> Process (Response b)) ->
           State -> ((a, Connection), RequestNumber) -> Process State
request h state ((a, conn), req) = do
    (resp, state') <- h state (a, conn)
    sendResponse conn req resp
    return state'

forward :: Serializable a => a -> ForwardData -> Process ()
forward parsed (areaPid, conn, req) = do
    evaluate parsed
    case req of
        0 -> send areaPid (parsed, conn)
        _ -> send areaPid ((parsed, conn), req)

parseClientCmd :: String -> Value -> ForwardData -> Process ()
parseClientCmd "echo" body = do
    let Success txt = fromJSON body :: Result String
    forward (Echo txt)
parseClientCmd "enter_area" body = do
    let Success aid = fromJSON body :: Result AreaId
    forward (EnterArea aid)
parseClientCmd "get_objects_info" body = do
    let Success ids = fromJSON body :: Result [String]
    forward (GetObjectsInfo ids)
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


clientCmd :: AreaPid -> String -> Value -> RequestNumber ->
             Connection -> Process ()
clientCmd (AreaPid pid) cmd body req conn =
    parseClientCmd cmd body (pid, conn, req)




