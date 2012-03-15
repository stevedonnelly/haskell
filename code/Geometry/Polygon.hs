
module Geometry.Polygon where
import Algebra.Matrix as M
import Algebra.Vector as V
import Control.Exception.Base
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ratio as Ratio
import Geometry.LineSegment as LS
import Geometry.Matrix2d as M2d
import Geometry.Vector2d as V2d
import Prelude.Extensions as PreludeExt


type Polygon = [V.Vector]

translate = \polygon translation -> (List.map (V.add translation) polygon)
rotate = \polygon angle -> (List.map (M.transform (M2d.rotation angle)) polygon)
transform = \polygon center angle translation -> let
    centered = (translate polygon (V.negate center))
    rotated = (Geometry.Polygon.rotate centered angle)
    translated = (translate rotated (V.add center translation))
    in translated

minimumYPoint = \points -> let
    preconditions = (notNull points)
    x = ((flip V.element) 0)
    y = ((flip V.element) 1)
    yLess = \a b -> ((<) (y a) (y b)) 
    xLess = \a b -> ((&&) ((==) (y a) (y b)) ((<) (x a) (x b)))
    isLowerLeft = \a b -> ((||) (yLess a b) (xLess a b))
    result = (List.foldr (select2 isLowerLeft) (List.head points) (List.tail points))
    in (assert preconditions result)

sortCounterClockwise = \points -> let
    minimum_y = (minimumYPoint points)
    x_axis = (V.fromList [1, 0])
    pointToAngle = \point -> let
        to_point = (V.subtract point minimum_y)
        length = (V.length to_point)
        angle = ((/) (dotProduct to_point x_axis) length)
        in (ifElse ((==) length 0) (1, point) (angle, point))
    in (snd (unzip (List.sortBy (flip compare) (List.map pointToAngle points))))

convexHull = \points -> let
    number_of_points = (List.length points)
    sorted = (sortCounterClockwise points)
    stack = [(head (tail sorted)), (head (sorted))]
    remaining = ((++) (tail (tail sorted)) [(head sorted)])
    isLeftTurn = \p0 p1 p2 -> let
        v01 = (V.subtract p1 p0)
        v02 = (V.subtract p2 p0)
        in ((>) (V2d.crossProduct v01 v02) 0)
    buildHull = \stack point -> let
        popRightTurns = \stack -> let
            one_in_stack = (List.null (tail stack))
            is_left = (isLeftTurn (head (tail stack)) (head stack) point)
            in (ifElse ((||) one_in_stack is_left) stack (popRightTurns (tail stack)))
        in ((:) point (popRightTurns stack))
    hull = (List.foldl buildHull stack remaining)
    in (List.reverse (List.tail hull))

pointsToEdges = \points -> (zip points (rotateLeft points))

edgeNormal = \(a, b) -> (V2d.perpendicular (V.subtract a b))
edgeNormalAngle = \(a, b) -> (V2d.toAngle (edgeNormal (a, b)))

-- edges are (a, b) point pairs, edge normal is defined from a to b, rotated right 
gaussianMap = \edges -> let
    in (Map.fromList (List.map (\edge -> ((edgeNormalAngle edge), edge)) edges))


compatibleVertices = \gaussian_map angle -> let
    toList = \(a, b) -> [a, b]
    (less, equal, greater) = (Map.splitLookup angle gaussian_map)
    parallel = [(fst (fromJust equal)), (snd (fromJust equal))]
    is_outside_map = ((||) (Map.null less) (Map.null greater))
    outside = (List.intersect (toList (snd (Map.findMax gaussian_map))) (toList (snd (Map.findMin gaussian_map))))
    inside = (List.intersect (toList (snd (Map.findMax less))) (toList (snd (Map.findMin greater))))
    in (ifElse (isJust equal) parallel (ifElse is_outside_map outside inside))


convexMinkowskiSumEdges = \a_edges b_edges -> let
    a_map = (gaussianMap a_edges)
    b_map = (gaussianMap b_edges)
    edgeVertices = \map edge -> (compatibleVertices map (edgeNormalAngle edge))
    edgeVertexFacets = \gaussian_map edge -> let
        vertices = (edgeVertices gaussian_map edge)
        in (List.map (\x -> ((V.add x (fst edge)), (V.add x (snd edge)))) vertices)
    in ((++) (concat (List.map (edgeVertexFacets b_map) a_edges)) (concat (List.map (edgeVertexFacets a_map) b_edges)))



convexPenetrationDepth = \a_points b_points -> let
    a_edges = (pointsToEdges a_points)
    b_negated_edges = (pointsToEdges (List.map V.negate b_points))
    minkowski_sum = (convexMinkowskiSumEdges a_edges b_negated_edges)
    origin = (V.zero 2)
    closerToOrigin = \a b -> let
        distance = \p -> (LS.distanceSquaredToPoint p origin)
        in ((<=) (distance a) (distance b))
    closest = (List.foldr (select2 closerToOrigin) (head minkowski_sum) (tail minkowski_sum))
    to_origin = (V.subtract origin (LS.closestPoint closest origin))
    is_inside = ((<=) (dotProduct to_origin (edgeNormal closest)) 0)
    in (is_inside, to_origin)

convexIntersection = \a_points b_points -> let
    (is_overlap, penetration) = (convexPenetrationDepth a_points b_points)
    b_edges = (pointsToEdges b_points)
    map = (gaussianMap b_edges)
    contact = (head (compatibleVertices map (V2d.toAngle penetration)))
    in (is_overlap, (contact, penetration))


setPrecision = \precision -> (List.map (V.setPrecision precision))
setPrecision10 = (Geometry.Polygon.setPrecision ((%) 1 10000000000))


