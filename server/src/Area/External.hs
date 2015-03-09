
module Area.External (enter, clientCmd, reconnect) where

import Data.Aeson(Value, Result(Success), fromJSON)
import Control.Distributed.Process hiding (reconnect)

import Utils (evaluate)
import Connection (Connection)
import qualified User.External as UE
import Types (UserId, UserPid(..), AreaId, AreaPid(..), RequestNumber)
import Area.Types


parseClientCmd :: String -> Value -> Result ClientCommand
parseClientCmd "echo" body = Echo `fmap` fromJSON body
parseClientCmd "enter-area" body = EnterArea `fmap` fromJSON body
parseClientCmd "get-objects-info" body = GetObjectsInfo `fmap` fromJSON body
parseClientCmd "move-to" body = MoveTo `fmap` fromJSON body
parseClientCmd "ignite" body = Ignite `fmap` fromJSON body
parseClientCmd "shoot" body = Shoot `fmap` fromJSON body


enter :: AreaId -> (AreaId -> UE.UserArea) ->
         UserPid -> Bool -> Connection -> Process ()
enter aid userArea userPid login conn =
    nsend aid (Enter (userArea aid) userPid login, conn)


clientCmd :: AreaPid -> String -> Value -> RequestNumber ->
             Connection -> Process ()
clientCmd (AreaPid pid) cmd body req conn = do
    let Success parsed = parseClientCmd cmd body
    evaluate parsed
    case req of
        0 -> send pid (parsed, conn)
        _ -> send pid ((parsed, conn), req)


reconnect :: AreaId -> UserId -> Connection -> Process ()
reconnect areaId userId conn =
    nsend areaId (Reconnection userId, conn)

