module Data.Set.Extensions where
import Control.Exception.Base as Exception
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Set as Set
import Prelude.Extensions as PreludeExt

notNull = ((.) not Set.null)

deleteMin :: Ord a => Set a -> Set a
deleteMin = \set -> (Set.delete (Set.findMin set) set)
deleteMax :: Ord a => Set a -> Set a
deleteMax = \set -> (Set.delete (Set.findMax set) set)

insertOrDelete :: Ord a => a -> Bool -> Set a -> Set a
insertOrDelete = \key insert set -> ((ifElse insert Set.insert Set.delete) key set)

intersections :: Ord a => [Set a] -> (Set a)
intersections = \sets -> let
    preconditions = (ListExt.notNull sets)
    result = (List.foldl Set.intersection (head sets) (tail sets))
    in (assert preconditions result)

toMap :: Ord a => (a -> b) -> (Set a) -> (Map a b)
toMap = \toValue set -> (Map.fromAscList (List.map (\x -> (x, toValue x)) (Set.toList set)))

toIdentityMap :: Ord a => (Set a) -> (Map a a)
toIdentityMap = (toMap id)


