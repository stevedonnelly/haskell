module Data.TrieMap.Extensions where
import Data.List as List
import Data.Map as Map
import Prelude.Extensions as PreludeExt

data TrieMap k a = TrieNode (Bool, a) (Map k (TrieMap k a))
nodeValue = \(TrieNode x m) -> x
nodeMap = \(TrieNode x m) -> x

empty = (TrieNode (False, undefined) Map.empty)

insert :: [k] -> a -> (TrieMap k a) -> (TrieMap k a)
insert = \keys value trie -> let
    key = (head keys)
    tree = (nodeMap trie)
    subtrie = (ifElse (Map.elem key tree) ((!) tree key) empty)
    recurse = (insert (tail keys) value subtrie)
    recurse_result = (TrieNode (nodeValue trie) (Map.insert key recurse tree))
    base = (TrieNode (True, value) (nodeTree trie))
    in (ifElse (List.null keys) base recurse)


