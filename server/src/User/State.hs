module User.State where

import Data.Sequence (Seq)

import Control.Distributed.Process.Extras (TagPool)

import Types (Ts, AreaId)
import User.Types
import qualified User.Settings as US
import qualified WS.Connection as C


data State = State {
    user :: !User,
    areas :: ![AreaId],
    settings :: !US.Settings,
    connection :: Maybe C.Connection,
    disconnectTs :: Maybe Ts,
    reqTagPool :: TagPool,
    userMessages :: Seq UserMessage
    }

