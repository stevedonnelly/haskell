module Data.TrieMap.Extensions where
import qualified Data.List as List
import qualified Data.Map as Map
import Prelude hiding (null)
import Prelude.Extensions as PreludeExt

data TrieMap k a = TrieNode (Bool, a) (Map.Map k (TrieMap k a)) deriving (Show, Eq)
nodeValue = \(TrieNode x m) -> x
nodeMap = \(TrieNode x m) -> m

null = \trie -> ((&&) (not (fst (nodeValue trie))) (Map.null (nodeMap trie)))

empty = (TrieNode (False, undefined) Map.empty)

insert :: Ord k => [k] -> a -> (TrieMap k a) -> (TrieMap k a)
insert = \keys value trie -> let
    (key, tree) = (head keys, nodeMap trie)
    subtrie = (ifElse (Map.member key tree) ((Map.!) tree key) empty)
    recurse = (insert (tail keys) value subtrie)
    recurse_result = (TrieNode (nodeValue trie) (Map.insert key recurse tree))
    base = (TrieNode (True, value) tree)
    in (ifElse (List.null keys) base recurse_result)

traverse :: Ord k => [k] -> (TrieMap k a) -> (TrieMap k a)
traverse = \key trie -> let
    recurse = (traverse (tail key) ((Map.!) (nodeMap trie) (head key)))
    in (ifElse (List.null key) trie recurse)

traverseIf :: Ord k => [k] -> (TrieMap k a) -> (Bool, TrieMap k a)
traverseIf = \key trie -> let
    recurse_condition = (Map.member (head key) (nodeMap trie))
    recurse_traverse = (traverseIf (tail key) ((Map.!) (nodeMap trie) (head key)))
    recurse = (ifElse recurse_condition recurse_traverse (False, trie))
    in (ifElse (List.null key) (True, trie) recurse)

member :: Ord k => [k] -> (TrieMap k a) -> Bool
member = \key trie -> let
    (found, subtrie) = (traverseIf key trie)
    in ((&&) found (fst (nodeValue subtrie)))
lookup :: Ord k => [k] -> (TrieMap k a) -> a
lookup = \key trie -> (snd (nodeValue (traverse key trie)))

findExtreme :: Ord k => (Map.Map k (TrieMap k a) -> (k, TrieMap k a)) -> (TrieMap k a) -> ([k], a)
findExtreme = \finder trie -> let
    ((is_value, value), tree) = (nodeValue trie, nodeMap trie)
    (extreme_key, extreme_subtrie) = (finder tree)
    (recurse_key, recurse_value) = (findExtreme finder extreme_subtrie)
    recurse_result = ((:) extreme_key recurse_key, recurse_value)
    in (ifElse (Map.null tree) ([], value) recurse_result)

findMin :: Ord k => TrieMap k a -> ([k], a)
findMin = (findExtreme Map.findMin)

findMax :: Ord k => TrieMap k a -> ([k], a)
findMax = (findExtreme Map.findMax)

delete :: Ord k => [k] -> (TrieMap k a) -> (TrieMap k a)
delete = \keys trie -> let
    (key, tree) = (head keys, nodeMap trie)
    recurse = (ifElse (Map.member key tree) (delete (tail keys) ((Map.!) tree key)) empty)
    recurse_map = (ifElse (null recurse) (Map.delete key tree) (Map.insert key recurse tree))
    recurse_result = (TrieNode (nodeValue trie) recurse_map)
    base = (TrieNode (False, undefined) tree)
    in (ifElse (List.null keys) base recurse_result)

toList :: Ord k => (TrieMap k a) -> [([k], a)]
toList = \trie -> let
    toList = \key_stack trie -> let
        current = (ifElse (fst (nodeValue trie)) [(List.reverse key_stack, snd (nodeValue trie))] [])
        mapToList = \(key, trie) -> (toList ((:) key key_stack) trie)
        recurse = (concat (List.map mapToList (Map.toList (nodeMap trie))))
        in ((++) current recurse)
    in (toList [] trie)

keys :: Ord k => (TrieMap k a) -> [[k]]
keys = ((.) (List.map fst) toList)
elems :: Ord k => (TrieMap k a) -> [a]
elems = ((.) (List.map snd) toList)


