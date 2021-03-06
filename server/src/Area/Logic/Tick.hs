{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Area.Logic.Tick (
    BroadcastData,
    calculateTick
    ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Prelude hiding (log, (.))
import Control.Monad (liftM, when, unless)
import Control.Monad.Writer (runWriter)
import Control.Category ((>>>), (.))
import Control.Monad.State.Strict (get, gets, modify)
import Data.Function ((&))
import Data.Foldable (foldlM, toList)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Data.Lens.Strict (access, modL, (~=), (+=), (%=))
import Data.Lens.Partial.Common (getPL, (^%=), (^-=), (^+=))
import Control.Distributed.Process
import Data.Aeson (Value, object, (.=))

import Types (Ts)
import qualified WS.Connection as C
import qualified Area.Settings as AS
import Area.Logic.Action (
    Active(applyActions),
    Action(
        MoveDistance, MoveCircularTrajectory,
        startTs, endTs, startPos, endPos
        ),
    actionsL
    )
import Area.State
import Area.Logic.Collision (
    Collidable, Collision, Collider,
    collider, findAllCollisions, collisionPair
    )
import Area.Logic.Signal (
    Signal(Appearance, Disappearance, MoveAsteroid, JumpToArea),
    AReason(..), DReason(..)
    )
import Area.Utils (distance)
import Area.Logic.Grid (formZones, groupByCells)
import Area.Logic.Misc (spawnUser)
import Area.Types (Object(..), Destroyable(..), Positioned(..), ObjId(..), Pos)
import qualified Area.Objects.User as U
import qualified Area.Objects.ControlPoint as CP
import Area.External (enter)
import qualified User.External as UE


data TimeTick = TimeTick deriving (Generic, Typeable)
instance Binary TimeTick


type BroadcastData = [(Value, [C.Connection])]


type BG = ([Value], [C.Connection]) --broadcast group


type CG = M.Map Pos (Set.Set Collider) --collision group


calculateTick :: Ts -> StateS (Maybe BroadcastData)
calculateTick ts = do
    previousTs <- gets currentTs
    modify $ \s -> s{currentTs=ts}
    handleActions
    handleCollisions
    checkDurability
    handleSignals
    tnum <- access tickNumberL
    tickNumberL += 1
    broadcastEveryTick <- gets $ AS.broadcastEveryTick . settings
    tickDurationHistorySize <- gets $ AS.tickDurationHistorySize . settings
    let broadcast = tnum `rem` broadcastEveryTick == 0
    tickDurationsL %= (take tickDurationHistorySize) . (ts - previousTs :)
    if broadcast
        then liftM Just (getBroadcastData ts)
        else return Nothing


handleActions :: StateS ()
handleActions = do
    ts <- gets currentTs
    let
        handleActive lens = do
            actives <- access lens
            signals <- access signalsL
            let
                (actives', signals') =
                    M.foldlWithKey handle (M.empty, signals) actives
            lens ~= actives'
            signalsL ~= signals'
            return ()
        handle (res, signals) ident active =
            let (active', newSignals) = runWriter $ applyActions ts active
                res' = M.insert ident active' res
            in (res', signals Seq.>< newSignals)
    handleActive $ usersL >>> usersDataL
    handleActive gatesL
    handleActive asteroidsL
    handleActive controlPointsL


handleCollisions :: StateS ()
handleCollisions = do
    cellSize <- gets $ AS.collisionCellSize . settings
    state <- get
    let group :: Collidable c => [c] -> CG -> CG
        group = groupByCells cellSize Set.empty $ flip $ Set.insert . collider
        g getter = group $ M.elems $ getter state
        collisionZones =
            formZones $ foldl (&) M.empty [
                g controlPoints,
                g asteroids,
                g $ usersData . users
                ]
        collisions = findAllCollisions collisionZones
    collidersL ~= collisionZones
    mapM_ handleCollision $ Set.toAscList collisions
    return ()


handleCollision :: Collision -> StateS()
handleCollision collision =
    case collisionPair collision of
        (UId uid, aid@AsteroidId{}) ->
            decreaseDurability (userFieldPL uid) (asteroidFieldPL aid)
        (aid@AsteroidId{}, aid'@AsteroidId{}) ->
            decreaseDurability (asteroidFieldPL aid) (asteroidFieldPL aid')
        (UId uid, UId uid') ->
            decreaseDurability (userFieldPL uid) (userFieldPL uid')
        (UId uid, cpid@CPId{}) ->
            decreaseDurability (userFieldPL uid) (cpFieldPL cpid)
        _ -> return ()
    where
        decreaseDurability l l' = do
            let durLens = l durabilityL
                durLens' = l' durabilityL
            mDurabiility <- gets $ getPL durLens
            mDurabiility' <- gets $ getPL durLens'
            case (mDurabiility, mDurabiility') of
                (Just dur, Just dur') -> do
                    let dur'' = min dur dur'
                    modify $ durLens ^-= dur''
                    modify $ durLens' ^-= dur''
                _ -> return ()


checkDurability :: StateS ()
checkDurability = do
    checkObjects $ usersL >>> usersDataL
    checkObjects asteroidsL
    checkObjects controlPointsL
    where
        checkObjects lens = M.elems <$> access lens >>= mapM_ check
        check obj =
            when (getDurability obj <= 0) $
                addSignalS $ Disappearance (objId obj) Destruction (getPos obj)


handleSignals :: StateS ()
handleSignals = do
    signals <- access signalsL
    signalsL ~= Seq.empty
    signals' <- foldlM handle Seq.empty signals
    signalsForBroadcastL %= (Seq.>< signals')
    return ()
    where
        handle res signal = do
            maybeSendSignal <- handleSignal signal
            return $
                case maybeSendSignal of
                    Nothing -> res
                    Just signal' -> res Seq.|> signal'


handleSignal :: Signal -> StateS (Maybe Signal)
handleSignal signal@(Disappearance (UId uid) Destruction _) = do
    mAttacker <- gets $ getPL $ userFieldPL uid U.lastAttackerL
    let
        resetUser u = u{
            U.durability=1,
            U.actions=[],
            U.deaths=U.deaths u + 1,
            U.lastAttacker=Nothing
            }
    modify $ userPL uid ^%= resetUser
    modify $ spawnUser uid Nothing
    Just pos <- gets $ getPL $ userFieldPL uid posL
    addSignalS $ Appearance uid Recovery pos
    case mAttacker of
        Just (Just attackerId) ->
            modify $ userFieldPL attackerId U.killsL ^+= 1
        _ -> return ()
    return $ Just signal
handleSignal signal@(Disappearance aid@(AsteroidId _) Destruction _) = do
    asteroidsL %= M.delete aid
    return $ Just signal
handleSignal (Disappearance cpid@(CPId _) Destruction _) = do
    modify $ cpPL cpid ^%= \cp -> cp{CP.durability=1, CP.owner=Nothing}
    return Nothing
handleSignal (MoveAsteroid aid targetPos) = do
    ts <- gets currentTs
    speed <- gets $ AS.asteroidPullSpeed . settings
    mPos <- gets $ getPL $ asteroidFieldPL aid posL
    let asLens = asteroidFieldPL aid actionsL


        movement MoveDistance{} = True
        movement MoveCircularTrajectory{} = True
        movement _ = False

        moveDistance MoveDistance{endPos=end}
            | end == targetPos = False
        moveDistance MoveDistance{} = True
        moveDistance _ = False

        astPos = fromJust mPos
        action = MoveDistance{
            startTs=ts,
            endTs=ts + round dt,
            startPos=astPos,
            endPos=targetPos
            }
        dt = distance astPos targetPos / (speed / 1000)
    modify $ asLens ^%= filter (not . moveDistance)
    mActions <- gets $ getPL asLens
    case mActions of
        Just as ->
            unless (any movement as) $
                modify $ asLens ^%= (action :)
        Nothing -> return ()
    return Nothing
handleSignal (JumpToArea aid uid) = do
    fromArea <- gets areaId
    maybeUser <- gets $ getPL $ userPL uid
    case maybeUser of
        Just user -> do
            let
                U.User{
                    U.pid=pid,
                    U.monitorRef=ref,
                    U.connection=conn,
                    U.pos=pos
                    } = user
            addSideEffectS $ do
                unmonitor ref
                UE.switchArea pid aid
                enter aid (U.userArea user) pid (Just fromArea) conn
            addSignalS $ Disappearance (UId uid) Exit pos
        Nothing -> return ()
    modify $ usersL `modL` deleteUser uid
    return Nothing
handleSignal signal = return $ Just signal


getBroadcastData :: Ts -> StateS BroadcastData
getBroadcastData ts = do
    aid <- gets areaId
    signals <- gets $ toList . signalsForBroadcast
    signalsForBroadcastL ~= Seq.empty
    connections <- gets $ M.keys . connectionIndex . users
    cellSize <- gets $ AS.broadcastCellSize . settings
    state <- get
    let g getter = map tickClientInfo $ M.elems $ getter state
        commonObjects = [
            g gates,
            g controlPoints
            ]
        commonPayload =
            object [
                "areaId" .= aid,
                "objects" .= concat commonObjects,
                "timestamp" .= ts
                ]

        reducer (objects, connections') obj =
            (tickClientInfo obj : objects, connections')
        userReducer (objects, connections') user =
            (tickClientInfo user : objects, U.connection user : connections')
        group ::
            Object o => (BG -> o -> BG) -> [o] -> M.Map Pos BG -> M.Map Pos BG
        group = groupByCells cellSize ([], [])

        gg getter = M.elems $ getter state
        zones =
            formZones $ foldl (&) M.empty [
                group reducer $ gg asteroids,
                group userReducer $ gg $ usersData . users
                ]

        signalCells = groupByCells cellSize [] (flip (:)) signals M.empty
        signalZones = M.map concat $ formZones signalCells

        broadcastZone (pos, zone@(center:_)) =
            let objects = fst center
                payload =
                    object [
                        "areaId" .= aid,
                        "objects" .= objects,
                        "signals" .= M.findWithDefault [] pos signalZones,
                        "timestamp" .= ts
                        ]
            in (payload, concat $ map snd zone)
        broadcastZone (_, []) = (object [], [])
        bd = map broadcastZone $ M.toList zones
        bd' = (commonPayload, connections) : bd
    return bd'



