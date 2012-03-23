
module Data.List.Extensions where
import Data.Ix as Ix
import Data.List as List
import Data.Map as Map
import Prelude.Extensions as PreludeExt


notNull = ((.) not List.null)

count :: Eq a => [a] -> a -> Int
count = \list x -> (List.length (List.filter ((==) x) list))

map2 = \function first second -> (List.map (uncurry function) (zip first second))

product = \as bs -> let
    zipRow = \a -> let
        zipCell = \b -> (a, b)
        in (List.map zipCell bs)
    in (List.map zipRow as)

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct = (curry ((.) concat (uncurry Data.List.Extensions.product)))

pushLast = \list x -> ((++) list [x])

dropLast = \list -> (reverse (tail (reverse list)))

rotateLeft = \list -> (pushLast (tail list) (head list))

rotateRight = \list -> ((:) (last list) (dropLast list))

replace = \list index value -> let
    (before, after) = (List.splitAt index list)
    in ((++) before ((:) value (tail after)))

removeIndices = \indices list -> let
    removeIndices = \indices list index -> let
        is_end = ((||) (List.null indices) (List.null list))
        is_match = ((==) (head indices) index)
        remove = (removeIndices (tail indices) (tail list) ((+) index 1))
        keep = ((:) (head list) (removeIndices indices (tail list) ((+) index 1)))
        in (ifElse is_end list (ifElse is_match remove keep))
    in (removeIndices indices list 0)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = \value list -> let
    splitter = \x (current, lists) -> (ifElse ((==) x value) ([], (:) current lists) ((:) x current, lists))
    (current, lists) = (List.foldr splitter ([], []) list)
    in ((:) current lists)

range = \low high -> (Ix.range (low, ((-) high 1)))

range0 = (Data.List.Extensions.range 0)

uniformSequence :: Num a => Ord a => a -> a -> a -> [a]
uniformSequence = \step min max -> 
    (ifElse ((<) min max) ((:) min (uniformSequence step ((+) min step) max)) [])

zipIndices = \start list -> let 
    size = (length list) 
    in (zip (Data.List.Extensions.range start ((+) start size)) list)

zipIndices0 = (zipIndices 0)

toArray start list = (Map.fromAscList (zipIndices start list))

toArray0 = (toArray 0)


