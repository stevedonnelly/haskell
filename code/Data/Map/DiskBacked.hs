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
    (return lru_handle)

putLRU = \lru_handle map -> do
    (putMVar (mapLRU map) lru_handle)

empty :: Ord k => Int -> (k -> Int) -> Int -> ((k, v) -> [Word8]) -> ([Word8] -> (k, v)) -> String -> IO (Map k v)
empty = \size keyIndex record_width serialize deserialize path -> do
    backing_file <- (openBinaryFile path ReadWriteMode)
    lru <- (newMVar (LRU.newLRU (Just (fromIntegral size)), backing_file)) 
    (return (lru, keyIndex, (+) record_width 1, serialize, deserialize))

maxSize :: Map k v -> IO Int
maxSize = \map -> do
    (lru, file) <- (takeLRU map)
    let size = (fromJust (LRU.maxSize lru))
    (putLRU (lru, file) map)
    (return (fromIntegral size))

readBackingFile :: Eq k => k -> (Map k v) -> Handle -> IO (Maybe v)
readBackingFile = \key map handle -> do
    let position = (keyFilePosition key map)
    let width = (mapRecordWidth map)
    (hSeek handle AbsoluteSeek position)
    ptr <- (mallocArray width)
    bytes_read <- (hGetBuf handle ptr width)
    bytes <- (peekArray width ptr)
    let eof = ((/=) bytes_read width)
    let flag = (ifElse eof 0 (List.head bytes))
    let (disk_key, disk_value) = ((mapDeserialize map) (List.tail bytes))
    (return (ifElse ((==) flag 1) (Just disk_value) Nothing))

writeToBackingFile :: Eq k => k -> v -> Map k v -> Handle -> IO ()
writeToBackingFile = \key value map handle -> do
    let position = (keyFilePosition key map)
    let width = (mapRecordWidth map)
    let serialized = ((:) 1 ((mapSerialize map) (key,value)))
    ptr <- (newArray serialized)
    (hSeek handle AbsoluteSeek position) 
    (hPutBuf handle ptr width)

unsetBackingFile :: Eq k => k -> (Map k v) -> Handle -> IO ()
unsetBackingFile = \key map handle -> do
    let position = (keyFilePosition key map)
    let bytes = [0] :: [Word8]
    ptr <- (newArray bytes)
    (hSeek handle AbsoluteSeek position) 
    (hPutBuf handle ptr 1)

insertWithLock :: Ord k => k -> v -> Map k v -> LRU k v -> Handle -> IO (LRU k v)
insertWithLock = \key value map lru backing_file -> do
    let (next, dropped) = (LRU.insertInforming key value lru)
    let (drop_key, drop_value) = (fromJust dropped)
    let write = (writeToBackingFile drop_key drop_value map backing_file)
    (ifElse (isJust dropped) write noop)
    (return next)

insert :: Ord k => k -> v -> Map k v -> IO ()
insert = \key value map -> do
    (lru, backing_file) <- (takeLRU map)
    updated_lru <- (insertWithLock key value map lru backing_file)
    (putLRU (updated_lru, backing_file) map)

insertToDisk :: Ord k => k -> v -> Map k v -> IO ()
insertToDisk = \key value map -> do
    (lru, backing_file) <- (takeLRU map)
    (writeToBackingFile key value map backing_file)
    (putLRU (lru, backing_file) map)

insertIfAbsent :: Ord k => k -> v -> Map k v -> IO (Maybe v)
insertIfAbsent = \key value map -> do
    (lru, backing_file) <- (takeLRU map)
    (updated_lru, previous_value) <- (lookupWithLock key map lru backing_file)
    let insertion = (insertWithLock key value map updated_lru backing_file)
    final_lru <- (ifElse (isNothing previous_value) insertion (return updated_lru))
    (putLRU (final_lru, backing_file) map)
    (return previous_value)

lookupWithLock :: Ord k => k -> Map k v -> (LRU k v) -> Handle -> IO (LRU k v, Maybe v)
lookupWithLock = \key map lru backing_file -> do
    let (updated, cached) = (LRU.lookup key lru)
    let {from_file = do
        file_value <- (readBackingFile key map backing_file)
        let refresh = (insertWithLock key (fromJust file_value) map updated backing_file)
        refreshed <- (ifElse (isJust file_value) refresh (return updated))
        (return (refreshed, file_value))}
    (ifElse (isJust cached) (return (updated, cached)) from_file)
    
lookup :: Ord k => k -> Map k v -> IO (Maybe v)
lookup = \key map -> do
    (lru, backing_file) <- (takeLRU map)
    (updated, value) <- (lookupWithLock key map lru backing_file) 
    (putLRU (updated, backing_file) map)
    (return value)
        
delete :: Ord k => k -> Map k v -> IO ()
delete = \key map -> do
    (lru, backing_file) <- (takeLRU map)
    let (updated, value) = (LRU.delete key lru)
    (ifElse (isNothing value) (unsetBackingFile key map backing_file) noop)
    (putLRU (updated, backing_file) map)


