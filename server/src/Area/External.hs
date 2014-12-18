
module Area.External (enter, clientCmd, reconnect) where

import Data.Aeson(Value, Result(Success), fromJSON)
import Control.Distributed.Process hiding (forward, reconnect)
import Control.Distributed.Process.Serializable (Serializable)

import Utils (evaluate)
import Connection (Connection)
import qualified User.External as UE
import Types (UserId, UserPid(..), AreaId, AreaPid(..), RequestNumber)
import Area.Types


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
parseClientCmd "enter-area" body = do
    let Success aid = fromJSON body :: Result AreaId
    forward (EnterArea aid)
parseClientCmd "get-objects-info" body = do
    let Success ids = fromJSON body :: Result [String]
    forward (GetObjectsInfo ids)
parseClientCmd "move-to" body = do
    let Success toPos = fromJSON body :: Result Pos
    forward (MoveTo toPos)
parseClientCmd "ignite" body = do
    let Success dmgSpeed = fromJSON body :: Result Float
    forward (Ignite dmgSpeed)



enter :: AreaId -> (AreaId -> UE.UserArea) ->
         UserPid -> Connection -> Process ()
enter aid userArea userPid conn = do
    Just areaPid <- whereis aid
    send areaPid (Enter (userArea aid) userPid, conn)


clientCmd :: AreaPid -> String -> Value -> RequestNumber ->
             Connection -> Process ()
clientCmd (AreaPid pid) cmd body req conn =
    parseClientCmd cmd body (pid, conn, req)


reconnect :: AreaId -> UserId -> Connection -> Process ()
reconnect areaId userId conn= do
    Just areaPid <- whereis areaId
    send areaPid (Reconnection userId, conn)

