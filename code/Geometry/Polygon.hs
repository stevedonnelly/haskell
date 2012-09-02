
module Geometry.Polygon where
import Algebra.Matrix as M
import Algebra.Vector as V
import Control.Exception.Base
import qualified Data.Graph.Extensions as Graph
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

pointsToEdges = \points -> (ListExt.map2 LS.fromEndpoints points (rotateLeft points))
edges = ((.) pointsToEdges points)

directedGraph = \polygon -> (Map.fromList (List.map (\e -> (endpoint0 e, [endpoint1 e])) (edges polygon)))

translate = \polygon translation -> (List.map (V.add translation) polygon)
rotate = \polygon angle -> (List.map (M.transform (M2d.rotation angle)) polygon)
transform = \polygon center angle translation -> let
    centered = (Geometry.Polygon.translate polygon (V.negate center))
    rotated = (Geometry.Polygon.rotate centered angle)
    translated = (Geometry.Polygon.translate rotated (V.add center translation))
    in translated

pointIntersection = \polygon point -> let
    walkBoundary = \(winding_number, on_boundary) edge -> let
        toQuadrant = ((.) V2d.quadrant (flip V.subtract point))
        [start, stop] = (List.map toQuadrant [endpoint0 edge, endpoint1 edge])
        quadrant = ((-) stop start)
        turn = (V2d.crossProduct (LS.direction edge) (V.subtract point (LS.endpoint0 edge)))
        turn_cases = [((>) turn 0, ((ifElse ((<) quadrant 0) ((+) quadrant 4) quadrant), False)),
            ((<) turn 0, ((ifElse ((>) quadrant 0) ((-) quadrant 4) quadrant), False))]
        (winding_change, boundary_change) = (cases turn_cases (0, LS.pointIntersection edge point))
        in ((+) winding_number winding_change, (||) on_boundary boundary_change)
    (winding_number, on_boundary) = (List.foldl walkBoundary (0, False) (edges polygon))
    in ((||) ((==) winding_number 4) on_boundary)

intersectionSubdivision :: Polygon -> (Map Int [Vector]) -> Polygon
intersectionSubdivision = \polygon intersection_lookup -> let
    edgeSubdivision = \(id, edge) -> let
        intersections = ((!) intersection_lookup id)
        scalar_points = (Map.fromList (List.map (\x -> (LS.projectionScalar edge x, x)) intersections))
        in (Map.elems (Map.delete 1 (Map.insert 0 (endpoint0 edge) scalar_points)))
    in (fromPoints (concat (List.map edgeSubdivision (zipIndices0 (edges polygon)))))

intersectionGraph :: Polygon -> Polygon -> (Graph.Graph Vector, Set Vector)
intersectionGraph = \polygon0 polygon1 -> let
    edge_pairs = (ListExt.crossProduct (zipIndices0 (edges polygon0)) (zipIndices0 (edges polygon1)))
    intersections = (List.map (\((id0, f0), (id1, f1)) -> (id0, id1, LS.intersection f0 f1)) edge_pairs)
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

extractPolygonCycle :: (Graph.Graph Vector) -> [Vector] -> [Vector] -> (Bool, [Vector])
extractPolygonCycle = \graph start path -> let
    (current, previous, neighbors) = (head path, head (tail path), (!) graph current)
    outwardsAngle = \x -> (V2d.positiveAngle (V.subtract previous current) (V.subtract x current))
    (angle, next) = (List.minimum (List.map (\x -> (outwardsAngle x, x)) neighbors))
    recurse = (extractPolygonCycle graph start ((:) next path))
    (prefix, suffix) = (List.splitAt 2 path)
    is_complete = ((&&) ((==) prefix start) (ListExt.notNull suffix))
    in (ifElse is_complete (True,List.reverse suffix) (ifElse (List.null neighbors) (False,path) recurse))

extractPolygonCycles :: (Graph.Graph Vector) -> [Polygon]
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

edgeNormal = ((.) V.negate ((.) V2d.perpendicular LS.direction))
edgeNormalQuadrantRatio = ((.) V2d.quadrantRatio edgeNormal)

-- assumes every edge has a unique normal angle; i.e. no quadrant-ratio keys will duplicated in the normal map
gaussianMap = \edges -> let
    normal_map = (Map.fromList (List.map (\edge -> (edgeNormalQuadrantRatio edge, edge)) edges))
    (((q0, r0), min), ((q1, r1), max)) = (Map.findMin normal_map, Map.findMax normal_map)
    wrapped_min = (Map.insert ((+) q0 4, r0) min normal_map)
    wrapped_max = (Map.insert ((-) q1 4, r1) max wrapped_min)
    in wrapped_max

compatibleVertices = \gaussian_map quadrant -> let
    (less, equal, greater) = (Map.splitLookup quadrant gaussian_map)
    parallel = [LS.endpoint0 (fromJust equal), LS.endpoint1 (fromJust equal)]
    result = [LS.endpoint1 (snd (Map.findMax less))]
    in (ifElse (isJust equal) parallel result)

-- assumes no sequential edges are parallel
convexMinkowskiSumEdges = \a_edges b_edges -> let
    a_map = (gaussianMap a_edges)
    b_map = (gaussianMap b_edges)
    edgeVertexEdges = \gaussian_map edge -> let
        vertices = (compatibleVertices gaussian_map (edgeNormalQuadrantRatio edge))
        in (List.map (LS.translate edge) vertices)
    in ((++) (concat (List.map (edgeVertexEdges b_map) a_edges)) (concat (List.map (edgeVertexEdges a_map) b_edges)))

-- assumes no sequential edges are parallel; this requirement can be ensured by first calling the convexHull function on each polygon
convexPenetrationDepth = \a_points b_points -> let
    a_edges = (pointsToEdges a_points)
    b_negated_edges = (pointsToEdges (List.map V.negate b_points))
    minkowski_sum = (convexMinkowskiSumEdges a_edges b_negated_edges)
    origin = (V.zero 2)
    distances_to_origin = (List.map (flip LS.distanceSquaredToPoint origin) minkowski_sum)
    closest = (snd (List.minimum (zip distances_to_origin minkowski_sum)))
    to_origin = (V.subtract origin (LS.closestPoint closest origin))
    is_inside = ((<) (dotProduct to_origin (edgeNormal closest)) 0)
    in (is_inside, to_origin)

convexPenetrationPoint = \a_points b_points -> let
    (is_overlap, penetration) = (convexPenetrationDepth a_points b_points)
    a_map = (gaussianMap (pointsToEdges a_points))
    b_map = (gaussianMap (pointsToEdges b_points))
    a_contacts = (compatibleVertices a_map (V2d.quadrantRatio (V.negate penetration)))
    b_contacts = (compatibleVertices b_map (V2d.quadrantRatio penetration))
    contact = (ifElse ((==) (List.length a_contacts) 1) (head a_contacts) (head b_contacts))
    in (is_overlap, contact, penetration)


setPrecision = \precision -> (List.map (V.setPrecision precision))
setPrecision10 = (Geometry.Polygon.setPrecision ((%) 1 10000000000))


