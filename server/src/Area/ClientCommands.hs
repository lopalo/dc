
module Area.ClientCommands where

import qualified Data.Map.Strict as M
import Data.List (elemIndex, maximumBy)

import Data.Aeson (ToJSON, Value, toJSON)
import Data.String.Utils (startswith)
import Data.Lens.Strict ((^%=), (^-=))
import Control.Distributed.Process

import Utils (milliseconds)
import Connection (Connection, sendResponse)
import Types (UserId, RequestNumber)
import qualified Settings as S
import qualified Area.User as U
import qualified User.External as UE
import Area.Action (Action(..))
import Area.Utils (distance, getIntervals)
import Area.Types
import Area.State
import Area.Signal (Signal(Disappearance, Shot), DReason(Exit))
import Area.External (enter)
import Area.Vector (toVect, add, sub, mul, lenSqr, len, dot)


type Response = Process (Value, State)

response :: ToJSON a => a -> State -> Response
response resp state = return (toJSON resp, state)

handleClientReq :: State -> ((ClientCommand, Connection), RequestNumber) ->
                   Process State
handleClientReq state ((a, conn), req) = do
    (resp, state') <- handleClientRequest state (a, conn)
    sendResponse conn req resp
    return state'


handleClientRequest :: State -> (ClientCommand, Connection) -> Response
handleClientRequest state (Echo txt, _) = do
    let resp = areaId state ++ " echo: " ++ txt
    response resp state

handleClientRequest state (GetObjectsInfo ids, _) = do
    let objects = foldl getObj [] ids
        us = usersData $ users state
        getObj :: [Value] -> String -> [Value]
        getObj res ident
            | "user-id:" `startswith` ident =
                let ident' = read ident :: UserId
                in case ident' `M.lookup` us of
                    Nothing -> res
                    Just user -> U.initClientInfo user : res
    response objects state


handleClientCommand :: State -> (ClientCommand, Connection) -> Process State
handleClientCommand state (EnterArea aid, conn) = do
    let user@U.User{U.userId=uid} = conn `userByConn` state
        userPid = (userPids . users) state M.! uid
        delUser = usersL ^%= deleteUser uid
        addSig = addSignal $ Disappearance (UId uid) Exit
    UE.switchArea userPid aid
    enter aid (U.userArea user) userPid False conn
    return $ addSig $ delUser state

handleClientCommand state (MoveAlongRoute route, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        user = userByConn conn state
        threshold = S.routeFilterThreshold $ settings state
        route' = filterRoute threshold (U.pos user : route)
        pathLength = sum $ map (uncurry distance) $ getIntervals route'
        dt = pathLength / (fromIntegral (U.speed user) / 1000)
        --TODO: if it's moving, consider current direction;
        --      otherwise rotate it before moving along the route
        action = MoveRoute{startTs=now,
                           endTs=now + round dt,
                           positions=route'}
        replace MoveRoute{} = True
        replace Rotation{} = True
        replace _ = False
        updActions = replaceUserAction uid replace action
    return $ updActions state


handleClientCommand state (Ignite dmgSpeed, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        action = Burning{previousTs=now,
                         damageSpeed=dmgSpeed}
    return $ addUserAction uid action state

handleClientCommand state (Shoot target, conn) =
    return $ addSig $ updUsr state
    where addSig = addSignal $ Shot (uidByConn conn state) target
          damage = S.shotDamage $ settings state
          updUsr = updateUser (U.durabilityL ^-= damage) target


userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connToIds . users) state M.! conn


updateUserActions :: ([Action] -> [Action]) -> UserId -> State -> State
updateUserActions f = updateUser (U.actionsL ^%= f)

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = updateUserActions (action :) uid

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = updateUserActions (filter (not . f)) uid

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f


filterRoute :: Float -> [Pos] -> [Pos]
--Ramer–Douglas–Peucker algorithm
filterRoute threshold = filterRoute'
    where
        filterRoute' [] = []
        filterRoute' [a] = [a]
        filterRoute' [a, b] = [a, b]
        filterRoute' ps' =
            let start = head ps'
                end = last ps'
                distPairs = map (distToSegment start end) ps'
                maxPair = maximumBy comp $ tail $ init distPairs
                Just index = elemIndex maxPair distPairs
                (left, right) = splitAt (index + 1) ps'
                (maxDist, point) = maxPair
                left' = filterRoute' left
                right' = tail $ filterRoute' $ point : right
            in
                if maxDist <= threshold
                    then [start, end]
                    else left' ++ right'
        comp a b = fst a `compare` fst b
        distToSegment start end point =
            let p = toVect point
                s = toVect start
                e = toVect end
                sp = p `sub` s
                se = e `sub` s
                ep = p `sub` e
                v = case sp `dot` se / lenSqr se of
                    t | start == end -> sp
                      | t < 0 -> sp
                      | t > 1 -> ep
                      | otherwise -> se `mul` t `add` s `sub` p
            in (len v, point)

