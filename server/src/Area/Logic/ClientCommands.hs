
module Area.Logic.ClientCommands (
    handleClientRequest,
    handleClientCommand
    ) where

import Data.Function ((&))
import qualified Data.Map.Strict as M
import Data.List (elemIndex, maximumBy)
import System.Random (randomR)
import Data.Fixed (mod')

import Data.Aeson (ToJSON, Value, toJSON)
import Data.Lens.Partial.Common ((^.), (^%=), (^-=))

import Utils (mkRandomGen)
import WS.Connection (Connection)
import Types (AreaId(AreaId), UserId, width)
import qualified Area.Settings as AS
import qualified Area.Objects.User as U
import qualified Area.Objects.ControlPoint as CP
import Area.Logic.Action (Action(..), actionsL, moveAction)
import Area.Utils (distance, getIntervals)
import Area.Types
import Area.State
import Area.Logic.Signal (Signal(Shot, JumpToArea))
import Area.Logic.Vector (
    fromPolar, toPos, fromPos, add, sub,
    angle, distanceToSegment
    )
import Area.Logic.Collision (Collision(Collision), rayCollision)


response :: ToJSON a => a -> State -> (Value, State)
response resp state = (toJSON resp, state)


handleClientRequest :: State -> (ClientCommand, Connection) -> (Value, State)
handleClientRequest state (Echo txt, _) =
    let resp = show (areaId state) ++ " echo: " ++ txt
    in response resp state

handleClientRequest state (GetObjectsInfo ids, _) =
    let objects = foldl getObj [] ids
        us = usersData $ users state
        gs = gates state
        as = asteroids state
        cps = controlPoints state
        getObj res objIdent =
            let
                get ident objs =
                    case ident `M.lookup` objs of
                        Nothing -> res
                        Just obj -> initClientInfo obj : res
            in
                case objIdent of
                    (UId uid) -> get uid us
                    gid@(GateId _) -> get gid gs
                    aid@(AsteroidId _) -> get aid as
                    aid@(CPId _) -> get aid cps
    in response objects state


handleClientCommand :: State -> (ClientCommand, Connection) -> State
handleClientCommand state (EnterArea aid, conn) =
    let sett = settings state
        uid = uidByConn conn state
        user = userByConn conn state
        dt = AS.jumpRotationMilliseconds sett
        globalPositions = AS.globalPositions sett
        AreaId fromArea = areaId state
        AreaId toArea = aid
        now = currentTs state
        start = globalPositions M.! fromArea
        end = globalPositions M.! toArea
        ang = U.angle user `mod'` 360
        ang' = angle $ fromPos end `sub` fromPos start
        da = ang' - ang
        ang'' =
            if abs da > 180
                then if da > 0 then ang' - 360 else ang' + 360
                else ang'
        action = Rotation{
            startTs=now,
            endTs=now + dt,
            startAngle=ang,
            endAngle=ang'',
            completeSignal=JumpToArea aid uid
            }
        updActions = replaceUserAction uid moveAction action
    in updActions state


handleClientCommand state (MoveAlongRoute route, conn) =
    let uid = uidByConn conn state
        user = userByConn conn state
        now = currentTs state
        pos = U.pos user
        speed = U.speed user

        moveV = fromPolar (fromIntegral speed) (U.angle user)
        pos' = toPos $ fromPos pos `add` moveV

        threshold = AS.routeFilterThreshold $ settings state
        route' = filterRoute threshold (pos : pos' : route)
        pathLength = sum $ map (uncurry distance) $ getIntervals route'
        dt = pathLength / (fromIntegral speed / 1000)
        action = MoveRoute{
            startTs=now,
            endTs=now + round dt,
            positions=route'
            }
        updActions = replaceUserAction uid moveAction action
    in updActions state

handleClientCommand state (Recover recSpeed, conn) =
    let uid = uidByConn conn state
        now = currentTs state
        action = Recovery{
            durabilityAccum=0,
            recoverySpeed=recSpeed,
            prevTs=now
            }
        replace Recovery{} = True
        replace _ = False
    in replaceUserAction uid replace action state

handleClientCommand state (Shoot targetPos, conn) =
    case state ^. userPL shooterId of
        Just shooter ->
            let sett = settings state
                cooldown = AS.shotCooldownMilliseconds $ AS.shot sett
                now = currentTs state

                shotAllowed = U.nextShootTs shooter <= now
                shotCooldown =
                    userPL shooterId ^%= \u ->
                        u{U.nextShootTs=now + cooldown}

                rGen = mkRandomGen shooterId $ tickNumber state
                (t, _) = randomR (-0.5, 0.5) rGen
                dWidth = t * fromIntegral (width $ U.size shooter)
                widthV = fromPolar dWidth $ U.angle shooter
                shooterPos = toPos $ fromPos (U.pos shooter) `add` widthV

                rayDist = AS.shotDistance $ AS.shot sett
                cellSize = AS.collisionCellSize sett
                rayAngle =
                    angle $ fromPos targetPos `sub` fromPos shooterPos
                rayV = fromPolar rayDist rayAngle
                targetPos' = toPos $ fromPos shooterPos `add` rayV
                sid = UId shooterId
                ray = rayCollision cellSize sid shooterPos targetPos'

                addSig pos = addSignal $ Shot shooterPos pos
                damage = AS.shotDamage $ AS.shot sett

                dmgTarget (UId uid) =
                    userFieldPL uid durabilityL ^-= damage
                dmgTarget aid@AsteroidId{} =
                    asteroidFieldPL aid durabilityL ^-= damage
                dmgTarget cpid@CPId{} =
                    cpFieldPL cpid durabilityL ^-= damage
                dmgTarget _ = id

                updTargetAttacker (UId uid) =
                    userPL uid ^%= \u -> u{U.lastAttacker=Just shooterId}
                updTargetAttacker _ = id

                replace Recovery{} = True
                replace _ = False
                updTargetActions (UId uid) = cancelUserAction uid replace
                updTargetActions _ = id
            in
                if shotAllowed
                    then
                        case ray $ colliders state of
                            Just (Collision _ targetId targetPos'') ->
                                foldl (&) state [
                                    addSig targetPos'',
                                    updTargetActions targetId,
                                    updTargetAttacker targetId,
                                    dmgTarget targetId,
                                    shotCooldown
                                    ]
                            Nothing ->
                                foldl (&) state [
                                    addSig targetPos',
                                    shotCooldown
                                    ]
                    else state
        Nothing -> state
    where shooterId = uidByConn conn state

handleClientCommand state (Capture cpid, conn) = state'
    where
        ownerId = uidByConn conn state
        name = U.name $ userByConn conn state
        capture cp =
            case CP.owner cp of
                Just _ -> cp
                Nothing -> cp{
                    CP.owner=Just (ownerId, name),
                    CP.durability=CP.maxDurability cp
                    }
        state' = (cpPL cpid ^%= capture) state

handleClientCommand state (PullAsteroid aid, conn) =
    let uid = uidByConn conn state
        action = PullingAsteroid{asteroidId=aid}
    in addUserAction uid action state

handleClientCommand state (CancelPull, conn) =
    let uid = uidByConn conn state
        cancel PullingAsteroid{} = True
        cancel _ = False
    in cancelUserAction uid cancel state


userByConn :: Connection -> State -> U.User
userByConn conn state = (usersData . users) state M.! uidByConn conn state


uidByConn :: Connection -> State -> UserId
uidByConn conn state = (connectionIndex . users) state M.! conn


modifyUserActions :: UserId -> ([Action] -> [Action]) -> State -> State
modifyUserActions uid f = userFieldPL uid actionsL ^%= f


addUserAction :: UserId -> Action -> State -> State
addUserAction uid action = modifyUserActions uid (action :)


cancelUserAction :: UserId -> (Action -> Bool) -> State -> State
cancelUserAction uid f = modifyUserActions uid (filter (not . f))


replaceUserAction :: UserId -> (Action -> Bool) -> Action -> State -> State
replaceUserAction uid f action =
    addUserAction uid action . cancelUserAction uid f


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

