{-# LANGUAGE OverloadedStrings #-}

module Admin.AreaHandlers (areaHandlers) where

import Data.Aeson (object, (.=))
import Control.Distributed.Process.Node (LocalNode)
import Control.Distributed.Process.Extras (newTagPool)
import Web.Scotty hiding (settings, status)

import Types (AreaStatus(..), AreaId(AreaId))
import Utils (execProcess)
import Area.External (getAreaStatus)


areaHandlers :: LocalNode -> ScottyM ()
areaHandlers node = do
    g "status" $ getStatus node
    where
        g = get . prfx
        p = post . prfx
        prfx = capture . ("/area/" ++)


getStatus :: LocalNode -> ActionM ()
getStatus node = do
    statusList <- execProcess node (getAreaStatus =<< newTagPool)
    json $ map statusView statusList
    where
        statusView status =
            object [
                "id" .= aid,
                "users" .= userAmount status,
                "objects" .= objectAmount status,
                "tick-duration-ms" .= tickDurationMs status
                ]
            where AreaId aid = statusAreaId status

