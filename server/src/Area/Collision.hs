
module Area.Collision where

import Data.List (minimumBy)
import qualified Data.Set as Set

import Data.Aeson (ToJSON, toJSON)

import Area.Types (Object, ObjId, Pos)
import Area.Vector


class Object o => Collidable o where

    collider :: o -> Collider


data Collider = Circular {objId :: ObjId,
                          position :: Pos,
                          radius :: Int}
              | Segment {objId :: ObjId,
                         startPosition :: Pos,
                         endPosition :: Pos}
              | Point {objId :: ObjId, position :: Pos}

instance Eq Collider where

    c == c' = objId c == objId c'

instance Ord Collider where

    c <= c' = objId c <= objId c'

type Colliders = Set.Set Collider


data Collision = Collision ObjId ObjId Pos

instance Eq Collision where

    c == c' = collisionPair c == collisionPair c'


instance Ord Collision where

    c <= c' = collisionPair c <= collisionPair c'

instance ToJSON Collision where

    toJSON _ = toJSON ""

type Collisions = Set.Set Collision

emptyColliders :: Colliders
emptyColliders = Set.empty

addCollider :: Collidable c => c -> Colliders -> Colliders
addCollider obj = Set.insert (collider obj)

collisionPair :: Collision -> (ObjId, ObjId)
collisionPair (Collision ident ident' _) = (ident, ident')


checkCollision :: Collider -> Collider -> Maybe Pos
checkCollision c c'
    | c == c' = Nothing
checkCollision c@Circular{} c'@Circular{} =
    let posV = toVect $ position c
        posV' = toVect $ position c'
        radius' = fromIntegral . radius
    in
        if len (posV `sub` posV') <= radius' c + radius' c'
            then Just $ fromVect $ posV `add` posV' `divide` 2
            else Nothing
checkCollision c@Segment{} c'@Circular{} =
    let start = startPosition c
        end = endPosition c
        point = position c'
        projection = projectionToSegment start end point
        distance = len $ projection `sub` toVect point
    in
        if distance <= fromIntegral (radius c')
            then Just $ fromVect projection
            else Nothing


findCollisions :: Collider -> Colliders -> Collisions
findCollisions coll = Set.foldl check Set.empty
    --TODO: split it to broad and narrow phases
    where
        check collisions coll' =
            case checkCollision coll coll' of
                Just pos ->
                    Set.insert
                        (Collision (objId coll) (objId coll') pos)
                        collisions
                Nothing -> collisions


rayCollision :: ObjId -> Pos -> Pos -> Colliders -> Maybe Collision
rayCollision ident start end colliders =
    let collisions = findCollisions (Segment ident start end) colliders
        startV = toVect start
        comp a b = dist a `compare` dist b
        dist (Collision _ _ p) = len (toVect p `sub` startV)
        nearest = minimumBy comp $ Set.toAscList collisions
    in
        if not $ Set.null collisions
            then Just nearest
            else Nothing
