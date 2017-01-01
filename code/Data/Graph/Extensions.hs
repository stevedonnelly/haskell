
module Data.Graph.Extensions where
import Data.List as List
import Data.Map as Map
import Data.Map.Extensions as MapExt
import Data.Maybe as Maybe 
import Data.Set as Set
import Data.Tuple as Tuple
import Prelude.Extensions as PreludeExt


type (Graph vertex) = (Map vertex [vertex])
type (SimpleGraph vertex) = (Map vertex (Set vertex))


empty = Map.empty

fromEdges :: Ord vertex => [(vertex, vertex)] -> (Graph vertex)
fromEdges = \vertex_neighbors -> (Map.fromListWith (++) (List.map (\(a,b) -> (a,[b])) vertex_neighbors))
edges = \graph -> let
    edge_pairs = (Map.mapWithKey (\key edges -> (List.map (\edge -> (key, edge)) edges)) graph)
    in (concat (Map.elems edge_pairs))

reverse :: Ord vertex => Graph vertex -> Graph vertex
reverse = \graph -> let
    reversed_edges = (List.map swap (edges graph))
    reversed_list = (List.map (\x -> (fst x, [snd x])) reversed_edges)
    in (Map.fromListWith (++) reversed_list)


depthFirstSearch :: Ord vertex => (Graph vertex) -> vertex -> (Map vertex (Int, Int))
depthFirstSearch = \graph start -> (snd (depthFirstSearchVisited graph start (0, Map.empty)))

depthFirstSearchAll :: Ord vertex => (Graph vertex) -> (Map vertex (Int, Int))
depthFirstSearchAll = \graph -> let
    startSearch = \previous start -> (depthFirstSearchVisited graph start previous)
    in (snd (List.foldl startSearch (0, Map.empty) (Map.keys graph)))

depthFirstSearchVisited :: Ord vertex => (Graph vertex) -> vertex -> (Int, Map vertex (Int, Int)) -> (Int, Map vertex (Int, Int))
depthFirstSearchVisited = \graph id time_visited -> let
    neighbors = ((!) graph id)
    time = (fst time_visited)
    visited = (snd time_visited)
    visited_current = (Map.insert id (time, -1) visited)
    visitNeighbor = \previous neighbor -> (depthFirstSearchVisited graph neighbor previous)
    neighbors_result = (List.foldl visitNeighbor (time + 1, visited_current) neighbors)
    exit_current = (Map.insert id (time, (fst neighbors_result)) (snd neighbors_result))
    exit_result = ((fst neighbors_result) + 1, exit_current)
    in (ifElse (Map.member id visited) time_visited exit_result)
        
connectedComponents :: Ord vertex => (Graph vertex) -> [[vertex]]
connectedComponents = \graph -> let
    connectedComponents = \time_vertices -> let
        ((start, stop), v) = (Map.findMin time_vertices)
        (less, greater) = (MapExt.splitLess (stop, stop) time_vertices)
        result = ((:) (Map.elems less) (connectedComponents greater))
        in (ifElse (Map.null time_vertices) [] result)
    in (connectedComponents (MapExt.injectiveInverse (depthFirstSearchAll graph)))

union :: Ord vertex => (Graph vertex) -> (Graph vertex) -> (Graph vertex)
union = \graph0 graph1 -> (Map.unionWith (++) graph0 graph1)


shortestPathWithHeuristic :: Ord node => Real cost => Show (node) => Show cost => node -> (node -> [node]) -> (node -> node -> cost) -> (node -> cost) -> (Map node cost -> Bool) -> (Map node cost, Map node node)
shortestPathWithHeuristic = \start_node neighbors cost heuristic completion -> let
    queueNeighbor = \node node_cost best (queue, queue_lookup) neighbor -> let
        neighbor_estimate = (sum [node_cost, cost node neighbor, heuristic neighbor])
        previous_estimate = (Map.lookup neighbor queue_lookup)
        updated_case = (Set.insert (neighbor_estimate, node, neighbor) queue, Map.insert neighbor neighbor_estimate queue_lookup)
        in (ifElse ((&&) (Map.notMember neighbor best)
                ((||) (Maybe.isNothing previous_estimate) ((<) neighbor_estimate (Maybe.fromJust previous_estimate))))
            updated_case
            (queue, queue_lookup))
    next = \(queue, queue_lookup, best, path) -> let
        ((estimated_cost, previous, node), remaining) = (Set.deleteFindMin queue)
        remaining_lookup = (Map.delete node queue_lookup)
        actual_cost = ((+) ((!) best previous) (cost previous node))
        updated_best = (Map.insert node actual_cost best)
        updated_path = (Map.insert node previous path)
        (updated_queue, updated_lookup) = (List.foldl (queueNeighbor node actual_cost updated_best) (remaining, remaining_lookup) (neighbors node))
        in (updated_queue, updated_lookup, updated_best, updated_path)
    condition = \(queue, queue_lookup, best, path) -> (completion best)
    (queue, queue_lookup) = (List.foldl (queueNeighbor start_node 0 Map.empty) (Set.empty, Map.empty) (neighbors start_node))
    initial = (queue, queue_lookup, Map.singleton start_node 0, Map.empty)
    (_, _, best, path) = (until condition next initial)
    in (best, path)


