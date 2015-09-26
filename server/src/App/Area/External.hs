
module App.Area.External (enter, clientCmd, reconnect) where

import Control.Applicative ((<$>))

import Data.Aeson (Value, Result(Success), fromJSON)
import Control.Distributed.Process hiding (reconnect)

import App.Utils (evaluate)
import App.Connection (Connection)
import qualified App.User.External as UE
import App.Types (UserId, UserPid(..), AreaId, AreaPid(..), RequestNumber)
import App.Area.Types


parseClientCmd :: String -> Value -> Result ClientCommand
parseClientCmd "echo" body = Echo <$> fromJSON body
parseClientCmd "enter-area" body = EnterArea <$> fromJSON body
parseClientCmd "get-objects-info" body = GetObjectsInfo <$> fromJSON body
parseClientCmd "move-along-route" body = MoveAlongRoute <$> fromJSON body
parseClientCmd "ignite" body = Ignite <$> fromJSON body
parseClientCmd "shoot" body = Shoot <$> fromJSON body


enter :: AreaId -> (AreaId -> UE.UserArea) ->
         UserPid -> Bool -> Connection -> Process ()
enter aid userArea userPid login conn =
    nsend aid (Enter (userArea aid) userPid login, conn)


clientCmd :: AreaPid -> String -> Value -> RequestNumber ->
             Connection -> Process ()
clientCmd (AreaPid pid) cmd body req conn = do
    --TODO: do pre-check for EnterErea. We must be sure that the area is alive
    let Success parsed = parseClientCmd cmd body
    evaluate parsed
    case req of
        0 -> send pid (parsed, conn)
        _ -> send pid ((parsed, conn), req)


reconnect :: AreaId -> UserId -> Connection -> Process ()
reconnect areaId userId conn =
    nsend areaId (Reconnection userId, conn)

