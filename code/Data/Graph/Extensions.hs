
module Data.Graph.Extensions where
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Tuple as Tuple
import Prelude.Extensions as PreludeExt


type (Graph vertex) = (Map vertex [vertex])
type (SimpleGraph vertex) = (Map vertex (Set vertex))


empty = Map.empty

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
        

