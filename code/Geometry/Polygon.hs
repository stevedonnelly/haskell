
module Geometry.Polygon where
import Algebra.Matrix as M
import Algebra.Vector as V
import Control.Exception.Base
import Data.Graph.Extensions as Graph
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ratio as Ratio
import Data.Set as Set
import Data.Set.Extensions as SetExt
import Data.Tuple.Extensions as TupleExt
import Geometry.AABB as AABB
import Geometry.LineSegment as LS
import Geometry.Matrix2d as M2d
import Geometry.Vector2d as V2d
import Prelude.Extensions as PreludeExt


type Polygon = [V.Vector]
fromPoints = id
points = id

faces = \polygon -> (ListExt.map2 LS.fromEndpoints (points polygon) (ListExt.rotateLeft (points polygon)))

directedGraph = \polygon -> (Map.fromList (List.map (\f -> (endpoint0 f, [endpoint1 f])) (faces polygon)))

translate = \polygon translation -> (List.map (V.add translation) polygon)
rotate = \polygon angle -> (List.map (M.transform (M2d.rotation angle)) polygon)
transform = \polygon center angle translation -> let
    centered = (Geometry.Polygon.translate polygon (V.negate center))
    rotated = (Geometry.Polygon.rotate centered angle)
    translated = (Geometry.Polygon.translate rotated (V.add center translation))
    in translated

pointIntersection = \polygon point -> let
    walkBoundary = \(winding_number, on_boundary) face -> let
        toQuadrant = ((.) V2d.quadrant (flip V.subtract point))
        [start, stop] = (List.map toQuadrant [endpoint0 face, endpoint1 face])
        quadrant = ((-) stop start)
        turn = (V2d.crossProduct (LS.direction face) (V.subtract point (LS.endpoint0 face)))
        turn_cases = [((>) turn 0, ((ifElse ((<) quadrant 0) ((+) quadrant 4) quadrant), False)),
            ((<) turn 0, ((ifElse ((>) quadrant 0) ((-) quadrant 4) quadrant), False))]
        (winding_change, boundary_change) = (cases turn_cases (0, LS.pointIntersection face point))
        in ((+) winding_number winding_change, (||) on_boundary boundary_change)
    (winding_number, on_boundary) = (List.foldl walkBoundary (0, False) (faces polygon))
    in ((||) ((==) winding_number 4) on_boundary)

intersectionSubdivision :: Polygon -> (Map Int [Vector]) -> Polygon
intersectionSubdivision = \polygon intersection_lookup -> let
    faceSubdivision = \(id, face) -> let
        intersections = ((!) intersection_lookup id)
        scalar_points = (Map.fromList (List.map (\x -> (LS.projectionScalar face x, x)) intersections))
        in (Map.elems (Map.delete 1 (Map.insert 0 (endpoint0 face) scalar_points)))
    in (fromPoints (concat (List.map faceSubdivision (zipIndices0 (faces polygon)))))

intersectionGraph :: Polygon -> Polygon -> (Graph Vector, Set Vector)
intersectionGraph = \polygon0 polygon1 -> let
    face_pairs = (ListExt.crossProduct (zipIndices0 (faces polygon0)) (zipIndices0 (faces polygon1)))
    intersections = (List.map (\((id0, f0), (id1, f1)) -> (id0, id1, LS.intersection f0 f1)) face_pairs)
    (take02, take12) = (\(a,b,c) -> (a,c), \(a,b,c) -> (b,c))
    subdivision0 = (intersectionSubdivision polygon0 (Map.fromListWith (++) (List.map take02 intersections)))
    subdivision1 = (intersectionSubdivision polygon1 (Map.fromListWith (++) (List.map take12 intersections)))
    intersection_set = (Set.fromList (concat (List.map third3 intersections)))
    inside0 = (List.filter (Geometry.Polygon.pointIntersection polygon1) (points polygon0))
    inside1 = (List.filter (Geometry.Polygon.pointIntersection polygon0) (points polygon1))
    inside_set = (Set.fromList (concat [inside0, inside1, concat (List.map third3 intersections)]))
    in (Graph.union (directedGraph subdivision0) (directedGraph subdivision1), inside_set)

filterIntersectionGraph = \graph inside_set polygon0 polygon1 -> let
    inside_neighbors = (Map.map (List.filter (flip Set.member inside_set)) graph)
    inside_vertices = (Map.intersection inside_neighbors (SetExt.toMap (const []) inside_set))
    insideGraph = \(a,b) -> let
        midpoint = (LS.midpoint (LS.fromEndpoints a b))
        pointIntersection = (flip Geometry.Polygon.pointIntersection midpoint)
        in ((&&) (pointIntersection polygon0) (pointIntersection polygon1))
    in (Graph.fromEdges (List.filter insideGraph (Graph.edges inside_vertices)))

extractPolygonCycle :: (Graph Vector) -> [Vector] -> [Vector] -> (Bool, [Vector])
extractPolygonCycle = \graph start path -> let
    (current, previous, neighbors) = (head path, head (tail path), (!) graph current)
    outwardsAngle = \x -> (V2d.positiveAngle (V.subtract previous current) (V.subtract x current))
    (angle, next) = (List.minimum (List.map (\x -> (outwardsAngle x, x)) neighbors))
    recurse = (extractPolygonCycle graph start ((:) next path))
    (prefix, suffix) = (List.splitAt 2 path)
    is_complete = ((&&) ((==) prefix start) (ListExt.notNull suffix))
    in (ifElse is_complete (True,List.reverse suffix) (ifElse (List.null neighbors) (False,path) recurse))

extractPolygonCycles :: (Graph Vector) -> [Polygon]
extractPolygonCycles = \graph -> let
    extractPolygonCycles = \graph -> let
        (start, neighbors) = (Map.findMax graph)
        outwardsAngle = \x -> (V2d.positiveAngle (V.fromList [1, 0]) (V.subtract x start))
        (angle, next) = (List.minimum (List.map (\x -> (outwardsAngle x, x)) neighbors))
        extracted_result = (extractPolygonCycle graph [next, start] [next, start])
        (is_cycle, path) = (ifElse (List.null neighbors) (True,[start]) extracted_result)
        notUsed = (flip Set.notMember (Set.fromList path))
        remaining_graph = (Map.filterWithKey (\k a -> notUsed k) (Map.map (List.filter notUsed) graph))
        recurse = (extractPolygonCycles remaining_graph)
        result = (ifElse is_cycle ((:) (fromPoints path) recurse) recurse)
        in (ifElse (Map.null graph) [] result)
    in (extractPolygonCycles graph)

intersection :: Polygon -> Polygon -> [Polygon]
intersection = \polygon0 polygon1 -> let
    (graph, inside_set) = (intersectionGraph polygon0 polygon1)
    inside_graph = (filterIntersectionGraph graph inside_set polygon0 polygon1)
    in (extractPolygonCycles inside_graph)

isLeftTurn = \p0 p1 p2 -> ((>) (V2d.crossProduct (V.subtract p1 p0) (V.subtract p2 p0)) 0)

convexHull = \points -> let
    sorted_points = (List.sort (List.map V.toList points))
    buildHull = \stack point -> let
        lessThanTwo = \stack -> ((||) (List.null stack) (List.null (tail stack)))
        turnsLeft = \stack -> (isLeftTurn ((!!) stack 1) ((!!) stack 0) point)
        convex_stack = (until (\x -> ((||) (lessThanTwo x) (turnsLeft x))) tail stack)
        in ((:) point convex_stack)
    bottom = (List.foldl buildHull [] sorted_points)
    top = (List.foldr (flip buildHull) [] sorted_points)
    in ((++) (List.reverse (tail bottom)) (List.reverse (tail top)))

pointsToEdges = \points -> (List.map (uncurry LS.fromEndpoints) (zip points (rotateLeft points)))

edgeNormal = ((.) V2d.perpendicular LS.direction)
edgeNormalAngle = ((.) V2d.toAngle edgeNormal)

--TODO: handle parallel faces (duplicate angle keys in map)
gaussianMap = \edges -> let
    normal_map = (Map.fromList (List.map (\edge -> (edgeNormalAngle edge, edge)) edges))
    (min, max) = (Map.findMin normal_map, Map.findMax normal_map)
    wrapped_min = (Map.insert ((+) (fst min) 360) (snd min) normal_map)
    wrapped_max = (Map.insert ((-) (fst max) 360) (snd max) wrapped_min)
    in wrapped_max

compatibleVertices = \gaussian_map angle -> let
    (less, equal, greater) = (Map.splitLookup angle gaussian_map)
    parallel = [LS.endpoint0 (fromJust equal), LS.endpoint1 (fromJust equal)]
    result = [LS.endpoint1 (snd (Map.findMax less))]
    in (ifElse (isJust equal) parallel result)

convexMinkowskiSumEdges = \a_edges b_edges -> let
    a_map = (gaussianMap a_edges)
    b_map = (gaussianMap b_edges)
    edgeVertexFacets = \gaussian_map edge -> let
        vertices = (compatibleVertices gaussian_map (edgeNormalAngle edge))
        in (List.map (LS.translate edge) vertices)
    in ((++) (concat (List.map (edgeVertexFacets b_map) a_edges)) (concat (List.map (edgeVertexFacets a_map) b_edges)))

convexPenetrationDepth = \a_points b_points -> let
    a_edges = (pointsToEdges a_points)
    b_negated_edges = (pointsToEdges (List.map V.negate b_points))
    minkowski_sum = (convexMinkowskiSumEdges a_edges b_negated_edges)
    origin = (V.zero 2)
    distances_to_origin = (List.map (flip LS.distanceSquaredToPoint origin) minkowski_sum)
    closest = (snd (List.minimum (zip distances_to_origin minkowski_sum)))
    to_origin = (V.subtract origin (LS.closestPoint closest origin))
    is_inside = ((<=) (dotProduct to_origin (edgeNormal closest)) 0)
    in (is_inside, to_origin)

--TODO: rewrite this, make return a 3-tuple
convexIntersection = \a_points b_points -> let
    (is_overlap, penetration) = (convexPenetrationDepth a_points b_points)
    b_edges = (pointsToEdges b_points)
    map = (gaussianMap b_edges)
    contact = (head (compatibleVertices map (V2d.toAngle penetration)))
    in (is_overlap, (contact, penetration))


setPrecision = \precision -> (List.map (V.setPrecision precision))
setPrecision10 = (Geometry.Polygon.setPrecision ((%) 1 10000000000))


