module Data.Map.DiskBacked where
import Control.Concurrent.MVar as MVar
import Data.Cache.LRU as LRU
import Data.List as List
import Data.Maybe as Maybe
import Data.Tuple.Extensions as TupleExt
import Data.Word as Word
import Foreign.Marshal.Array as Array
import Prelude.Extensions as PreludeExt
import System.IO as SysIO

type Map k v = (
    MVar (LRU k v, Handle),
    k -> Int,
    Int,
    (k,v) -> [Word8],
    [Word8] -> (k,v))
mapLRU = first5
mapKeyIndex = second5
mapRecordWidth = third5
mapSerialize = fourth5
mapDeserialize = fifth5

keyFilePosition :: k -> Map k v -> Integer
keyFilePosition = \key map -> (toInteger ((*) ((mapKeyIndex map) key) (mapRecordWidth map)))

takeLRU = \map -> do
    lru_handle <- (takeMVar (mapLRU map))
    return lru_handle

putLRU = \lru_handle map -> do
    (putMVar (mapLRU map) lru_handle)

empty :: Ord k => Int -> (k -> Int) -> Int -> ((k, v) -> [Word8]) -> ([Word8] -> (k, v)) -> String -> IO (Map k v)
empty = \size keyIndex record_width serialize deserialize path -> do
    backing_file <- (openBinaryFile path ReadWriteMode)
    lru <- (newMVar (LRU.newLRU (Just (fromIntegral size)), backing_file)) 
    return (lru, keyIndex, (+) record_width 1, serialize, deserialize)

--fromList :: Ord key => Maybe Integer -> [(key, val)] -> IO (AtomicLRU key val)
--toList :: Ord key => AtomicLRU key val -> IO [(key, val)]

maxSize :: Map k v -> IO Int
maxSize = \map -> do
    (lru, file) <- (takeLRU map)
    let size = (fromJust (LRU.maxSize lru))
    (putLRU (lru, file) map)
    return (fromIntegral size)

readBackingFile :: Eq k => k -> (Map k v) -> Handle -> IO (Maybe v)
readBackingFile = \key map handle -> do
    let position = (keyFilePosition key map)
    let width = (mapRecordWidth map)
    (hSeek handle AbsoluteSeek position)
    ptr <- (mallocArray width)
    let {search = do
        (hGetBuf handle ptr width)
        bytes <- (peekArray width ptr)
        let is_set = ((&&) ((==) (List.length bytes) width) ((==) (List.head bytes) 1))
        let (disk_key, disk_value) = ((mapDeserialize map) (List.tail bytes))
        (ifElse is_set
            (ifElse ((==) disk_key key)
                (return (Just disk_value))
                search)
            (return Nothing))}
    search

writeToBackingFile :: Eq k => k -> v -> Map k v -> Handle -> IO ()
writeToBackingFile = \key value map handle -> do
    let width = (mapRecordWidth map)
    let serialized = ((:) 1 ((mapSerialize map) (key,value)))
    ptr <- (newArray serialized)
    maybe_value <- (readBackingFile key map handle) 
    (hSeek handle RelativeSeek (toInteger ((-) 0 width)))
    (hPutBuf handle ptr width)

unsetBackingFile :: Eq k => k -> (Map k v) -> Handle -> IO ()
unsetBackingFile = \key map handle -> do
    let width = (mapRecordWidth map)
    maybe_value <- (readBackingFile key map handle) 
    (hSeek handle RelativeSeek (toInteger ((-) 0 width)))
    let bytes = [0] :: [Word8]
    ptr <- (newArray bytes)
    (hPutBuf handle ptr 1)

insertWithLock :: Ord k => k -> v -> Map k v -> LRU k v -> Handle -> IO (LRU k v)
insertWithLock = \key value map lru backing_file -> do
    let (next, dropped) = (LRU.insertInforming key value lru)
    let (drop_key, drop_value) = (fromJust dropped)
    let write = (writeToBackingFile drop_key drop_value map backing_file)
    (ifElse (isJust dropped) write noop)
    return next

insert :: Ord k => k -> v -> Map k v -> IO (Map k v)
insert = \key value map -> do
    (lru, backing_file) <- (takeLRU map)
    updated_lru <- (insertWithLock key value map lru backing_file)
    (putLRU (updated_lru, backing_file) map)
    (return map)
     
lookup :: Ord k => k -> Map k v -> IO (Maybe v)
lookup = \key map -> do
    (lru, backing_file) <- (takeLRU map)
    let (updated, cached) = (LRU.lookup key lru)
    let {from_file = do
        file_value <- (readBackingFile key map backing_file)
        let refresh = (insertWithLock key (fromJust file_value) map updated backing_file)
        refreshed <- (ifElse (isJust file_value) refresh (return updated))
        return (refreshed, file_value)}
    (final_cache, value) <- (ifElse (isJust cached) (return (updated, cached)) from_file)
    (putLRU (final_cache, backing_file) map)
    return value
        
delete :: Ord k => k -> Map k v -> IO (Map k v)
delete = \key map -> do
    (lru, backing_file) <- (takeLRU map)
    let (updated, value) = (LRU.delete key lru)
    (ifElse (isNothing value) (unsetBackingFile key map backing_file) noop)
    (putLRU (updated, backing_file) map)
    (return map)    


