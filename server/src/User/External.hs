{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module User.External(
    UserArea(..), SyncState(..), SwitchArea(..),
    monitorUser, syncState, switchArea,
    clientCmd, userBroadcastHandlers
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))

import Data.Aeson (FromJSON, Value, Result(Success), fromJSON)
import Control.Distributed.Process

import Base.Broadcaster (prepareHandler)
import WS.Connection (Connection)
import Utils (evaluate)
import Types
import User.Types hiding (User)


data UserArea = UserArea {
    userId :: UserId,
    name :: UserName,
    speed :: Int, --units per second
    maxDurability :: Int,
    durability :: Int,
    size :: !Size,
    kills :: !Int,
    deaths :: !Int
    }
    deriving (Generic, Typeable)

instance Binary UserArea


newtype SyncState = SyncState UserArea deriving (Generic, Typeable)

instance Binary SyncState


monitorUser :: UserPid -> Process UserMonitorRef
monitorUser (UserPid pid) = monitor pid


syncState :: UserPid -> UserArea -> Process ()
syncState (UserPid pid) user = send pid $ SyncState user


switchArea :: UserPid -> AreaId -> Process ()
switchArea (UserPid pid) aid = send pid $ SwitchArea aid


parseClientCmd :: String -> Value -> Result ClientCommand
parseClientCmd "send-message" =
    load $ \(uids, msg) -> SendUserMessage uids msg


load :: FromJSON a => (a -> b) -> Value -> Result b
load constructor body = constructor <$> fromJSON body


clientCmd ::
    UserPid -> String -> Value -> RequestNumber -> Connection -> Process ()
clientCmd (UserPid pid) cmd body req _ = do
    let Success parsed = parseClientCmd cmd body
    evaluate parsed
    case req of
        0 -> send pid parsed
        _ -> send pid (parsed, req)


handleBroadcastUserMessage ::
    (UserMessage -> Process ()) -> UserMessage -> Process ()
handleBroadcastUserMessage = ($)


userBroadcastHandlers :: [Match ()]
userBroadcastHandlers = [
    prepareHandler handleBroadcastUserMessage
    ]


