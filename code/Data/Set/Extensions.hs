
module Data.Set.Extensions where
import Control.Exception.Base as Exception
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Set as Set
import Prelude.Extensions as PreludeExt


intersections :: Ord a => [Set a] -> (Set a)
intersections = \sets -> let
    preconditions = (ListExt.notNull sets)
    result = (List.foldl Set.intersection (head sets) (tail sets))
    in (assert preconditions result)

toMap :: Ord a => (a -> b) -> (Set a) -> (Map a b)
toMap = \toValue set -> (Map.fromAscList (List.map (\x -> (x, toValue x)) (Set.toList set)))

toIdentityMap :: Ord a => (Set a) -> (Map a a)
toIdentityMap = (toMap id)


