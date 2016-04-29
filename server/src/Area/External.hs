{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Area.External (
    enter, clientCmd, reconnect,
    getOwners, getAreaStatus
    ) where

import Control.Applicative ((<$>))
import qualified Data.Map.Strict as M

import Data.Aeson (FromJSON, Value(Null), Result(Success), fromJSON)
import Control.Distributed.Process hiding (reconnect)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Extras (TagPool)

import Utils (evaluate)
import WS.Connection (Connection)
import Base.GlobalRegistry (globalNSend, multicallByPrefix)
import qualified User.External as UE
import Types (
    UserId, UserPid(..), AreaId,
    AreaPid(..), ServiceType(Area),
    RequestNumber, AreaOwners, AreaStatus,
    prefix
    )
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


distributedRequest ::
    (Serializable a, Serializable b) => a -> TagPool -> Process [b]
distributedRequest = multicallByPrefix $ prefix Area


getOwners :: TagPool -> Process AreaOwners
getOwners tagPool = M.fromList <$> distributedRequest GetOwner tagPool


getAreaStatus :: TagPool -> Process [AreaStatus]
getAreaStatus = distributedRequest GetAreaStatus
