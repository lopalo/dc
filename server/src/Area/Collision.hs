
module Area.Collision where

import Data.List (foldl', minimumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Data.Aeson (ToJSON, toJSON)

import Area.Types (Object, ObjId, Pos(Pos), Positioned(..))
import Area.Vector
import Area.Grid (cellPos)


class Object o => Collidable o where

    collider :: o -> Collider


data Collider
    = Circular {
        objId :: ObjId,
        position :: Pos,
        radius :: Int
        }
    | Segment {
        objId :: ObjId,
        startPosition :: Pos,
        endPosition :: Pos
        }
    | Point {objId :: ObjId, position :: Pos}

instance Eq Collider where

    c == c' = objId c == objId c'

instance Ord Collider where

    c <= c' = objId c <= objId c'


instance Positioned Collider where

    getPos segment@Segment{} = startPosition segment
    getPos c = position c

    setPos _ c = c


type Colliders = M.Map Pos [Set.Set Collider]


data Collision = Collision ObjId ObjId Pos

instance Eq Collision where

    c == c' = collisionPair c == collisionPair c'

instance Ord Collision where

    c <= c' = collisionPair c <= collisionPair c'

instance ToJSON Collision where

    toJSON _ = toJSON ""


type Collisions = Set.Set Collision


emptyColliders :: Colliders
emptyColliders = M.empty


collisionPair :: Collision -> (ObjId, ObjId)
collisionPair (Collision ident ident' _) = (ident, ident')


checkCollision :: Collider -> Collider -> Maybe Pos
checkCollision c c'
    | c == c' = Nothing
checkCollision c@Circular{} c'@Circular{} =
    let posV = fromPos $ position c
        posV' = fromPos $ position c'
        radius' = fromIntegral . radius
    in
        if len (posV `sub` posV') <= radius' c + radius' c'
            then Just $ toPos $ posV `add` posV' `divide` 2
            else Nothing
checkCollision c@Segment{} c'@Circular{} =
    let start = startPosition c
        end = endPosition c
        point = position c'
        projection = projectionToSegment start end point
        distance = len $ projection `sub` fromPos point
    in
        if distance <= fromIntegral (radius c')
            then Just $ toPos projection
            else Nothing


insertCollision ::
    Collider -> Set.Set Collision -> Collider -> Set.Set Collision
insertCollision coll collisions coll' =
    case checkCollision coll coll' of
        Just pos ->
            Set.insert (Collision (objId coll) (objId coll') pos) collisions
        Nothing -> collisions


findAllCollisions :: Colliders -> Collisions
findAllCollisions colliderGroups = foldl' Set.union Set.empty groupCollisions
    where
        groupCollisions = map checkGroup $ M.elems colliderGroups
        checkGroup field@(center:_) =
            let colliders = foldl' Set.union Set.empty field
                checkCollider collisions' coll =
                    Set.foldl (insertCollision coll) collisions' colliders
            in Set.foldl checkCollider Set.empty center
        checkGroup [] = Set.empty


findColliderCollisions :: Int -> Collider -> Colliders -> Collisions
findColliderCollisions cellSize coll cellColliders =
    Set.foldl (insertCollision coll) Set.empty colliders
    where
        positions =
            case coll of
                Segment{startPosition=Pos sx sy, endPosition=Pos ex ey} ->
                    let rx = range (ex - sx)
                        ry = range (ey - sy)
                        range delta =
                            let end = delta `quot` cellSize + unit
                                unit = signum delta
                            in map (* cellSize) [-unit, 0 .. end]
                    in [Pos (sx + ix) (sy + iy) | ix <- rx, iy <- ry]
                _ -> [position coll]
        findCellGroup = flip (M.findWithDefault []) cellColliders
        findCellGroup' = findCellGroup . cellPos cellSize
        cells = concat $ map findCellGroup' positions
        colliders = foldl' Set.union Set.empty cells


rayCollision :: Int -> ObjId -> Pos -> Pos -> Colliders -> Maybe Collision
rayCollision cellSize ident start end colliders =
    let segment = Segment ident start end
        collisions = findColliderCollisions cellSize segment colliders
        startV = fromPos start
        comp a b = dist a `compare` dist b
        dist (Collision _ _ p) = len (fromPos p `sub` startV)
        nearest = minimumBy comp $ Set.toAscList collisions
    in
        if not $ Set.null collisions
            then Just nearest
            else Nothing
