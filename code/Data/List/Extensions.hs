
module Data.List.Extensions where
import Control.DeepSeq as DeepSeq
import Control.DeepSeq.Extensions as DeepSeqExt
import Data.Ix as Ix
import Data.List as List
import Data.Map as Map
import Prelude.Extensions as PreludeExt


notNull = ((.) not List.null)

count :: Eq a => [a] -> a -> Int
count = \list x -> (List.length (List.filter ((==) x) list))

singleton = \x -> [x]

map2 = \function first second -> (List.map (uncurry function) (zip first second))

foldl'' :: NFData a => NFData b => (a -> b -> a) -> a -> [b] -> a
foldl'' = \function initial list -> let
    next = (function initial (head list))
    result = (deepseq next (foldl'' function next (tail list)))
    in (ifElse (List.null list) initial result)

product = \as bs -> let
    zipRow = \a -> let
        zipCell = \b -> (a, b)
        in (List.map zipCell bs)
    in (List.map zipRow as)

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct = (curry ((.) concat (uncurry Data.List.Extensions.product)))

crossProducts :: [[a]] -> [[a]]
crossProducts = \lists -> let
    recurse = (crossProducts (tail lists))
    current = (crossProduct (head lists) recurse)
    result = (List.map (uncurry (:)) current)
    in (ifElse (List.null lists) [[]] result)

pushLast = \list x -> ((++) list [x])

dropLast = \list -> (reverse (tail (reverse list)))

rotateLeft = \list -> (pushLast (tail list) (head list))

rotateRight = \list -> ((:) (last list) (dropLast list))

replace = \list index value -> let
    (before, after) = (List.splitAt index list)
    in ((++) before ((:) value (tail after)))

removeIndices :: [Int] -> [a] -> [a]
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

allEqual :: Eq a => [a] -> Bool
allEqual = \list -> ((||) (List.null list) (List.all ((==) (head list)) (tail list)))

indentWith :: String -> String -> String
indentWith = \tab string -> let
    in (unlines (List.map ((++) tab) (lines string)))
    
indent :: String -> String
indent = indentWith "\t"


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


