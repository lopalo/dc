
module Area.ClientCommands where

import qualified Data.Map.Strict as M
import Data.List (elemIndex, maximumBy)
import qualified Data.Set as Set

import Data.Aeson (ToJSON, Value, toJSON)
import Data.String.Utils (startswith)
import Data.Lens.Strict ((^%=), (^-=))
import Control.Distributed.Process

import Utils (milliseconds)
import Connection (Connection, sendResponse)
import Types (UserId, RequestNumber)
import qualified Area.Settings as AS
import qualified Area.Objects.User as U
import qualified User.External as UE
import Area.Action (Action(..), actionsL, moveAction)
import Area.Utils (distance, getIntervals)
import Area.Types
import Area.State
import Area.Signal (Signal(Disappearance, Shot), DReason(Exit))
import Area.External (enter)
import Area.Vector (toVect, add, sub, mul, lenSqr, len, dot, distanceToSegment)
import Area.Collision (Collision(Collision), rayCollision)


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
    let resp = show (areaId state) ++ " echo: " ++ txt
    response resp state

handleClientRequest state (GetObjectsInfo ids, _) = do
    let objects = foldl getObj [] ids
        us = usersData $ users state
        gs = gates state
        as = asteroids state
        getObj :: [Value] -> ObjId -> [Value]
        getObj res objId =
            let get ident objs =
                    case ident `M.lookup` objs of
                        Nothing -> res
                        Just obj -> initClientInfo obj : res
            in
                case objId of
                    (UId uid) -> get uid us
                    gid@(GateId _) -> get gid gs
                    aid@(AsteroidId _) -> get aid as
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
        threshold = AS.routeFilterThreshold $ settings state
        route' = filterRoute threshold (U.pos user : route)
        pathLength = sum $ map (uncurry distance) $ getIntervals route'
        dt = pathLength / (fromIntegral (U.speed user) / 1000)
        --TODO: if it's moving, consider current direction;
        --      otherwise rotate it before moving along the route
        action = MoveRoute{startTs=now,
                           endTs=now + round dt,
                           positions=route'}
        updActions = replaceUserAction uid moveAction action
    return $ updActions state


handleClientCommand state (Recover recSpeed, conn) = do
    now <- liftIO milliseconds
    let uid = uidByConn conn state
        action = Recovery{durabilityAccum=0,
                          recoverySpeed=recSpeed}
        replace Recovery{} = True
        replace _ = False
    return $ replaceUserAction uid replace action state

handleClientCommand state (Shoot targetPos, conn) = do
    let shooterId = uidByConn conn state
    return $ case userField shooterId U.pos state of
        Just shooterPos -> do
            let ray = rayCollision (UId shooterId) shooterPos targetPos

                addSig pos = addSignal $ Shot shooterPos pos
                damage = AS.shotDamage $ settings state

                dmgTarget (UId uid) =
                    modifyUser uid $ durabilityL ^-= damage
                dmgTarget aid@AsteroidId{} =
                    modifyAsteroid aid $ durabilityL ^-= damage
                dmgTarget _ = id

                updTargetAttacker (UId uid) =
                    modifyUser uid $ \u -> u{U.lastAttacker=Just shooterId}
                updTargetAttacker _ = id

                replace Recovery{} = True
                replace _ = False
                updTargetActions (UId uid) = cancelUserAction uid replace
                updTargetActions _ = id

            case ray $ colliders state of
                Just (Collision _ targetId targetPos') ->
                    let fs = [dmgTarget targetId,
                              updTargetAttacker targetId,
                              updTargetActions targetId,
                              addSig targetPos']
                    in foldr ($) state fs
                Nothing -> addSig targetPos state
        Nothing -> state



userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state

uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connToIds . users) state M.! conn


modifyUserActions :: UserId -> ([Action] -> [Action]) -> State -> State
modifyUserActions uid f = modifyUser uid (actionsL ^%= f)

addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = modifyUserActions uid (action :)

cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = modifyUserActions uid (filter (not . f))

replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action = addUserAction uid action
                               . cancelUserAction uid f


filterRoute :: Float -> [Pos] -> [Pos]
--Ramer-Douglas-Peucker algorithm
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
            (distanceToSegment start end point, point)
