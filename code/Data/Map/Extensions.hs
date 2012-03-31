
module Data.Map.Extensions where
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Prelude.Extensions as PreludeExt


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


