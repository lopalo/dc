
module Area.ClientCommands (handleClientCommand, handleClientReq) where

import Data.Aeson (Value)
import Control.Distributed.Process

import WS.Connection (Connection, sendResponse)
import Types (RequestNumber)
import qualified Area.Logic.ClientCommands as L
import Area.Types
import Area.State
import Area.Misc (updateOwnerName)


handleClientReq ::
    State -> ((ClientCommand, Connection), RequestNumber) -> Process State
handleClientReq state ((cmd, conn), req) = do
    (resp, state') <- handleClientRequest state (cmd, conn)
    sendResponse conn req resp
    return state'


handleClientRequest ::
    State -> (ClientCommand, Connection) -> Process (Value, State)
handleClientRequest state input =
    return $ L.handleClientRequest state input


handleClientCommand :: State -> (ClientCommand, Connection) -> Process State
handleClientCommand state (cmd@(Capture _), conn) = do
    let state' = L.handleClientCommand state (cmd, conn)
    updateOwnerName state'
    return state'

handleClientCommand state (cmd, conn) = do
    return $ L.handleClientCommand state (cmd, conn)
