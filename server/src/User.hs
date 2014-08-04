module User (AreaUserInfo) where

import Types (UserId, UserName)

--TODO: create separate type and add more properties: durability, speed and etc.
type AreaUserInfo = (UserId, UserName)
