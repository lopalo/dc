
module Area.State where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad (void)
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M

import Data.Lens.Strict (lens, Lens, (^%=), (%=))

import Connection (Connection)
import Types (UserId, UserPid, AreaId)
import qualified Area.User as U
import Area.Signal (Signal, Signals)
import Area.Settings (Settings)



data Users = Users {connections :: !Connections,
                    connToIds :: !ConnToIds,
                    usersData :: !UsersData,
                    userPids :: !UserPids,
                    userPidToIds :: !UserPidToIds}

type UsersData = M.Map UserId U.User
type Connections = M.Map UserId Connection
type ConnToIds = M.Map Connection UserId
type UserPids = M.Map UserId UserPid
type UserPidToIds = M.Map UserPid UserId

data State = State {areaId :: !AreaId,
                    settings :: !Settings,
                    tickNumber :: !Int,
                    users :: !Users,
                    signalBuffer :: !Signals,
                    signalsForBroadcast :: !Signals}

type StateS a = S.State State a

tickNumberL :: Lens State Int
tickNumberL = lens tickNumber (\v s -> s{tickNumber=v})

usersL :: Lens State Users
usersL = lens users (\v s -> s{users=v})

usersDataL :: Lens Users UsersData
usersDataL = lens usersData (\v us -> us{usersData=v})

connToIdsL :: Lens Users ConnToIds
connToIdsL = lens connToIds (\v us -> us{connToIds=v})

connectionsL :: Lens Users Connections
connectionsL = lens connections (\v us -> us{connections=v})

userPidsL :: Lens Users UserPids
userPidsL = lens userPids (\v us -> us{userPids=v})


userPidToIdsL :: Lens Users UserPidToIds
userPidToIdsL = lens userPidToIds (\v us -> us{userPidToIds=v})


signalsL :: Lens State Signals
signalsL = lens signalBuffer (\v s -> s{signalBuffer=v})

signalsForBroadcastL :: Lens State Signals
signalsForBroadcastL = lens signalsForBroadcast
                            (\v s -> s{signalsForBroadcast=v})



addUser :: UserId -> Connection -> UserPid -> U.User -> Users -> Users
addUser uid conn userPid user = (usersDataL ^%= M.insert uid user)
                              . (connToIdsL ^%= M.insert conn uid)
                              . (connectionsL ^%= M.insert uid conn)
                              . (userPidsL ^%= M.insert uid userPid)
                              . (userPidToIdsL ^%= M.insert userPid uid)


updateUser :: (U.User -> U.User) -> UserId -> State -> State
updateUser f uid = usersDataL . usersL ^%= M.adjust f uid


deleteUser :: UserId -> Users -> Users
deleteUser uid us = fun us
    where conn = connections us M.! uid
          pid = userPids us M.! uid
          fun = (usersDataL ^%= M.delete uid)
              . (connectionsL ^%= M.delete uid)
              . (connToIdsL ^%= M.delete conn)
              . (userPidsL ^%= M.delete uid)
              . (userPidToIdsL ^%= M.delete pid)


addSignal :: Signal -> State -> State
addSignal signal = signalsL ^%= (signal :)


addSignalS :: Signal -> StateS ()
addSignalS signal = void $ signalsL %= (signal :)
