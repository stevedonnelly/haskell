module Data.Map.Concurrent.DiskBacked where
import Data.List as List
import Data.List.Extensions as ListExt
import qualified Data.Map as ShardMap
import qualified Data.Map.DiskBacked as DataMap
import Data.Tuple.Extensions as TupleExt
import Data.Word as Word

type (Map k v) = (
    ShardMap.Map Int (DataMap.Map k v),
    k -> Int)
mapShardMap = first2
mapShardFunction = second2

selectShardMap :: Ord k => (Map k v) -> k -> (DataMap.Map k v)
selectShardMap = \map key -> ((ShardMap.!) (mapShardMap map) ((mapShardFunction map) key))

empty :: Ord k => Int -> Int -> (k -> Int) -> Int -> ((k, v) -> [Word8]) -> ([Word8] -> (k, v)) -> String -> IO (Map k v)
empty = \shards size keyIndex record_width serialize deserialize base_path -> do
    let shardFunction = (\key -> (mod (keyIndex key) shards))
    let adjustedKeyFunction = (\key -> (div (keyIndex key) shards))
    let shard_ids = (ListExt.range0 shards)
    let paths = (List.map (\id -> (concat [base_path, "-", show id])) shard_ids)
    data_maps <- (mapM (DataMap.empty size adjustedKeyFunction record_width serialize deserialize) paths)
    (return (ShardMap.fromList (zip shard_ids data_maps), shardFunction))

insertWith :: Ord k => (k -> v -> DataMap.Map k v -> IO a) -> k -> v -> Map k v -> IO a
insertWith = \dmInsert key value map -> do
    let shard = (selectShardMap map key)
    (dmInsert key value shard)
     
insert :: Ord k => k -> v -> Map k v -> IO ()
insert = (insertWith DataMap.insert)

insertIfAbsent :: Ord k => k -> v -> Map k v -> IO (Maybe v)
insertIfAbsent = (insertWith DataMap.insertIfAbsent)  
     
lookup :: Ord k => k -> Map k v -> IO (Maybe v)
lookup = \key map -> do
    let shard = (selectShardMap map key)
    (DataMap.lookup key shard)
        
delete :: Ord k => k -> Map k v -> IO ()
delete = \key map -> do
    let shard = (selectShardMap map key)
    (DataMap.delete key shard)

