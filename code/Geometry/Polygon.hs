
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

minimumYPoint = \points -> let
    preconditions = (ListExt.notNull points)
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


