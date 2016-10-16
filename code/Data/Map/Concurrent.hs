module Data.Map.Concurrent where
import Control.Applicative
import Control.Concurrent.ReadWriteLock as RWLock
import Data.IORef as IORef
import qualified Data.List as List
import qualified Data.List.Extensions as ListExt
import qualified Data.Map as Map
import qualified Data.Map.Extensions as MapExt
import Data.Maybe as Maybe
import Data.Tuple.Extensions as TupleExt
import Prelude hiding (lookup)
import Prelude.Extensions as PreludeExt

type (Map key value) = (
    Map.Map Int (RWLock, IORef (Map.Map key value)),
    Int,
    key -> Int)
shardMap = first3
shards = second3
hash = third3


empty :: Int -> (k -> Int) -> IO (Map k v)
empty = \shards hash -> do 
    let {submap = do
        lock <- RWLock.new
        shard <- (IORef.newIORef Map.empty)
        (return (lock, shard))}
    submaps <- (mapM id (List.replicate shards submap))
    (return (Map.fromList (zip (ListExt.range0 shards) submaps), shards, hash))

selectShard :: k -> (Map k v) -> (RWLock, IORef (Map.Map k v))
selectShard = \key map -> ((Map.!) (shardMap map) (mod (hash map key) (shards map)))

lookup :: Ord k => k -> (Map k v) -> IO (Maybe v)
lookup = \key map -> do
    let (lock, shard_ref) = (selectShard key map)
    (RWLock.acquireRead lock)
    shard <- (IORef.readIORef shard_ref)
    let result = (Map.lookup key shard)
    (RWLock.releaseRead lock)
    (return result)

(!) :: Ord k => (Map k v) -> k -> IO v
(!) = \map key -> (liftA fromJust (lookup key map))

lookupIf :: Ord k => k -> (Map k v) -> IO (Bool, v)
lookupIf = \key map -> (liftA splitMaybe (lookup key map))

insert :: Ord k => k -> v -> (Map k v) -> IO ()
insert = \key value map -> do
    let (lock, shard_ref) = (selectShard key map)
    (RWLock.acquireWrite lock)
    shard <- (IORef.readIORef shard_ref)
    let inserted = (Map.insert key value shard)
    (IORef.writeIORef shard_ref inserted)
    (RWLock.releaseWrite lock)

insertIfAbsent :: Ord k => k -> v -> (Map k v) -> IO (Maybe v)
insertIfAbsent = \key value map -> do
    previous <- (lookup key map)
    let {try_insert = do
        let (lock, shard_ref) = (selectShard key map)
        (RWLock.acquireWrite lock)
        shard <- (IORef.readIORef shard_ref)
        let (result, inserted) = (MapExt.insertIfAbsent key value shard)
        (IORef.writeIORef shard_ref inserted)
        (RWLock.releaseWrite lock)
        (return result)}
    (ifElse (isJust previous) (return previous) try_insert)

delete :: Ord k => k -> (Map k v) -> IO ()
delete = \key map -> do
    let (lock, shard_ref) = (selectShard key map)
    (RWLock.acquireWrite lock)
    shard <- (IORef.readIORef shard_ref)
    let deleted = (Map.delete key shard)
    (IORef.writeIORef shard_ref deleted)
    (RWLock.releaseWrite lock)


