
module Data.Map.Extensions where
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Maybe as Maybe
import Debug.Trace as Trace
import Prelude.Extensions as PreludeExt


notNull :: Map a b -> Bool
notNull = ((.) not Map.null)

lookupIf :: Ord a => a -> (Map a b) -> (Bool, b)
lookupIf = \key map -> (splitMaybe (Map.lookup key map)) 

insertIfAbsent :: Ord a => a -> b -> (Map a b) -> (Maybe b, Map a b)
insertIfAbsent = \key value map -> let
    result = (Map.lookup key map)
    in (result, ifElse (isJust result) map (Map.insert key value map))

inverse :: Ord a => Ord b => (Map a b) -> (Map b [a])
inverse = \map -> let
    swap = \(a, b) -> (b, [a])
    pairs = (List.map swap (Map.toList map))
    in (Map.fromListWith (++) pairs)

injectiveInverse :: Ord a => Ord b => (Map a b) -> (Map b a)
injectiveInverse = \map -> (Map.map List.head (inverse map))

deleteWithSelector :: Ord k => ((Map k a) -> (k, a)) -> (Map k a) -> (Map k a)
deleteWithSelector = \selector map -> (Map.delete (fst (selector map)) map)

deleteMin :: Ord k => (Map k a) -> (Map k a)
deleteMin = (deleteWithSelector Map.findMin)
deleteMax :: Ord k => (Map k a) -> (Map k a)
deleteMax = (deleteWithSelector Map.findMax)

splitLess :: Ord k => k -> (Map k a) -> (Map k a, Map k a)
splitLess = \key map -> let
    (less, equal, greater) = (Map.splitLookup key map)
    in (less, ifElse (isJust equal) (Map.insert key (fromJust equal) greater) greater)

splitLessEqual :: Ord k => k -> (Map k a) -> (Map k a, Map k a)
splitLessEqual = \key map -> let
    (less, equal, greater) = (Map.splitLookup key map)
    in (ifElse (isJust equal) (Map.insert key (fromJust equal) less) less, greater)

mapKeysAndValues :: Ord k1 => Ord k2 => ((k1, v1) -> (k2, v2)) -> (Map.Map k1 v1) -> (Map.Map k2 v2)
mapKeysAndValues = \transform map -> let
    in (Map.fromList (List.map transform (Map.toList map)))

mapKeys :: Ord k1 => Ord k2 => (k1 -> k2) -> (Map.Map k1 v) -> (Map.Map k2 v)
mapKeys = \transform map -> let
    pairTransform = \(k, v) -> (transform k, v)
    in (mapKeysAndValues pairTransform map)

memoize :: Ord a => (a -> b) -> (a -> Map a b -> (b, Map a b))
memoize = \function -> let
    memoized = \input cache -> let
        (is_cached, cached) = (lookupIf input cache) 
        output = (function input)
        in (ifElse is_cached (cached, cache) (output, Map.insert input output cache))
    in memoized

findExtremeWithDefault :: Ord k => (Map k a -> (k, a)) -> (k, a) -> Map k a -> (k, a)
findExtremeWithDefault = \selector thedefault map -> (ifElse (Map.null map) thedefault (selector map)) 

findMinWithDefault :: Ord k => (k, a) -> Map k a -> (k, a)
findMinWithDefault = (findExtremeWithDefault Map.findMin)

findMaxWithDefault :: Ord k => (k, a) -> Map k a -> (k, a)
findMaxWithDefault = (findExtremeWithDefault Map.findMax)

conflicts :: Ord k => Eq a => (Map k a) -> (Map k a) -> [k]
conflicts = \a b -> let
    intersection = (Map.intersectionWith (++) (Map.map ListExt.singleton a) (Map.map ListExt.singleton b))
    disagreements = (Map.filter ((.) not ListExt.allEqual) intersection)
    in (Map.keys disagreements)

hasConflicts :: Ord k => Eq a => (Map k a) -> (Map k a) -> Bool
hasConflicts = \a b -> (ListExt.notNull (conflicts a b))

fromKeyList :: Ord k => (k -> a) -> [k] -> (Map k a)
fromKeyList = \f keys -> (Map.fromList (List.map (\k -> (k, f k)) keys))

showLines :: (Show k, Show a) => (Map k a) -> String
showLines = ((.) unlines ((.) (List.map show) Map.toList))

debugLookup :: (Ord k, Show k, Show a) => (Map k a) -> k -> a
debugLookup = \map key -> let
    error_output = (concat ["\nfailed to find key:\n", show key, "\n\nin map:\n", showLines map, "\n\n"])
    trace_output = (ifElse (Map.member key map) "" error_output)
    in (Trace.trace trace_output ((!) map key))


