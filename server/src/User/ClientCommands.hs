
module User.ClientCommands (handleClientCommand) where
import Control.Distributed.Process

import User.Types
import User.State
import Base.GlobalRegistry (globalMultiWhereIs)
import Broadcaster (broadcast)


handleClientCommand :: State -> ClientCommand -> Process State
handleClientCommand state (SendUserMessage uids message) = do
    targets <- globalMultiWhereIs (map show uids) (reqTagPool state)
    broadcast [pid | Just pid <- targets] payload
    return state
    where
        usr = user state
        payload = UserMessage (userId usr) (name usr) message

