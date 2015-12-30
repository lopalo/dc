
module Area.External (enter, clientCmd, reconnect) where

import Control.Applicative ((<$>))

import Data.Aeson (FromJSON, Value(Null), Result(Success), fromJSON)
import Control.Distributed.Process hiding (reconnect)

import Utils (evaluate)
import Connection (Connection)
import GlobalRegistry (globalNSend)
import qualified User.External as UE
import Types (UserId, UserPid(..), AreaId, AreaPid(..), RequestNumber)
import Area.Types


parseClientCmd :: String -> Value -> Result ClientCommand
parseClientCmd "echo" = load Echo
parseClientCmd "enter-area" = load EnterArea
parseClientCmd "get-objects-info" = load GetObjectsInfo
parseClientCmd "move-along-route" = load MoveAlongRoute
parseClientCmd "recover" = load Recover
parseClientCmd "shoot" = load Shoot
parseClientCmd "capture" = load Capture
parseClientCmd "pull-asteroid" = load PullAsteroid
parseClientCmd "cancel-pull" = load $ \Null -> CancelPull


load :: FromJSON a => (a -> b) -> Value -> Result b
load constructor body = constructor <$> fromJSON body


enter :: AreaId -> UE.UserArea -> UserPid -> Bool -> Connection -> Process ()
enter aid userArea userPid login conn =
    globalNSend (show aid) (Enter userArea userPid login, conn)


clientCmd ::
    AreaPid -> String -> Value -> RequestNumber -> Connection -> Process ()
clientCmd (AreaPid pid) cmd body req conn = do
    let Success parsed = parseClientCmd cmd body
    evaluate parsed
    case req of
        0 -> send pid (parsed, conn)
        _ -> send pid ((parsed, conn), req)


reconnect :: AreaId -> UserId -> Connection -> Process ()
reconnect areaId userId conn =
    globalNSend (show areaId) (Reconnection userId, conn)

