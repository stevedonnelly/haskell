
module Data.Queue.Extensions where
import Data.Map as Map
import Prelude.Extensions as PreludeExt

type (Queue key0 key1 value) = (Map key0 (Map key1 value))

first :: Ord key0 => Ord key1 => Queue key0 key1 value -> value
first = \queue -> (snd (findMin (snd (findMin queue))))


rest :: Ord key0 => Ord key1 => Queue key0 key1 value -> Queue key0 key1 value
rest = \queue -> let
    (priority, map) = (findMin queue)
    remaining = (delete (fst (findMin map)) map)
    is_null = (Map.null remaining)
    with_remaining = (Map.insert priority remaining queue)
    without_remaining = (delete priority queue)
    in (ifElse is_null without_remaining with_remaining)


popFirst :: Ord key0 => Ord key1 => Queue key0 key1 value -> (value, Queue key0 key1 value)
popFirst = \queue -> (first queue, rest queue)


insertWith :: Ord key0 => Ord key1 => 
    Queue key0 key1 value -> key0 -> value ->
    (Map key1 value -> value -> Map key1 value) ->
    Queue key0 key1 value
insertWith = \queue priority value tieBreaker -> let
    has_tie = (Map.member priority queue)
    ties = (ifElse has_tie ((!) queue priority) Map.empty)
    merged = (tieBreaker ties value)
    in (Map.insert priority ties queue)

insert :: Ord key0 => Ord key1 => Num key1 => 
    Queue key0 key1 value -> key0 -> value ->
    Queue key0 key1 value
insert = \queue priority value -> let
    afterTies = \ties value -> let
        is_empty = (Map.null ties)
        after = (Map.insert ((+) (fst (findMax ties)) 1) value ties)
        only = (Map.singleton 0 value)
        in (ifElse is_empty only after)
    in (Data.Queue.Extensions.insertWith queue priority value afterTies)



