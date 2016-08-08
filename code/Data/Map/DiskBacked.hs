module Data.Map.DiskBacked where
import Control.Concurrent.MVar as MVar
import Data.Cache.LRU as LRU
import Data.Generics.Aliases as Generics
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

isNextBlank :: Handle -> IO Bool
isNextBlank = \handle -> do
    ptr <- (mallocArray 1)
    bytes_read <- (hGetBuf handle ptr 1)
    bytes <- (peekArray 1 ptr) :: IO [Word8]
    let eof = ((/=) bytes_read 1)
    let flag = (ifElse eof 0 (List.head bytes))
    (ifElse eof (return ()) (hSeek handle RelativeSeek (-1)))
    (return ((==) flag 0))

unsetPreviousPosition :: (Map k v) -> Handle -> IO ()
unsetPreviousPosition = \map handle -> do
    let width = (mapRecordWidth map)
    is_next_blank <- (isNextBlank handle)
    let bytes = [ifElse is_next_blank 0 2] :: [Word8]
    ptr <- (newArray bytes)
    (hSeek handle RelativeSeek (toInteger ((-) 0 width)))
    (hPutBuf handle ptr 1)

readBackingFile :: Eq k => k -> (Map k v) -> Handle -> IO (Maybe v, Maybe Integer)
readBackingFile = \key map handle -> do
    let position = (keyFilePosition key map)
    let width = (mapRecordWidth map)
    (hSeek handle AbsoluteSeek position)
    ptr <- (mallocArray width)
    let {search = \blank -> do
        current_position <- (hTell handle) 
        bytes_read <- (hGetBuf handle ptr width)
        bytes <- (peekArray width ptr)
        let eof = ((/=) bytes_read width)
        let flag = (ifElse eof 0 (List.head bytes))
        let updated_blank = (orElse blank (ifElse (List.elem flag [0, 2]) (Just current_position) Nothing))
        let (disk_key, disk_value) = ((mapDeserialize map) (List.tail bytes))
        let found_key = ((&&) ((==) flag 1) ((==) disk_key key))
        (ifElse eof (hSeek handle RelativeSeek (toInteger width)) (return ()))
        (ifElse found_key
            (return (Just disk_value, updated_blank))
            (ifElse ((==) flag 0)
                (return (Nothing, updated_blank))
                (search updated_blank)))}
    (search Nothing)

writeToBackingFile :: Eq k => k -> v -> Map k v -> Handle -> IO ()
writeToBackingFile = \key value map handle -> do
    let width = (mapRecordWidth map)
    let serialized = ((:) 1 ((mapSerialize map) (key,value)))
    ptr <- (newArray serialized)
    (maybe_value, first_blank) <- (readBackingFile key map handle)
    (ifElse (isJust maybe_value)
        (hSeek handle RelativeSeek (toInteger ((-) 0 width)))
        (hSeek handle AbsoluteSeek (fromJust first_blank)))
    (hPutBuf handle ptr width)

unsetBackingFile :: Eq k => k -> Map k v -> Handle -> IO ()
unsetBackingFile = \key map handle -> do
    (value, _) <- (readBackingFile key map handle)
    (ifElse (isJust value) (unsetPreviousPosition map handle) (return ()))

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

insertIfAbsent :: Ord k => k -> v -> Map k v -> IO (Maybe v)
insertIfAbsent = \key value map -> do
    (lru, backing_file) <- (takeLRU map)
    (updated_lru, previous_value) <- (lookupWithLock key map lru backing_file)
    let insertion = (insertWithLock key value map updated_lru backing_file)
    final_lru <- (ifElse (isNothing previous_value) insertion (return updated_lru))
    (putLRU (final_lru, backing_file) map)
    (return previous_value)

lookupWithLock :: Ord k => k -> Map k v -> LRU k v -> Handle -> IO (LRU k v, Maybe v)
lookupWithLock = \key map lru backing_file -> do
    let (updated, cached) = (LRU.lookup key lru)
    let {from_file = do
        (file_value, _) <- (readBackingFile key map backing_file)
        let {reinsert = do
            (unsetPreviousPosition map backing_file)
            (insertWithLock key (fromJust file_value) map updated backing_file)}
        refreshed <- (ifElse (isJust file_value) reinsert (return updated))
        return (refreshed, file_value)}
    (final_cache, value) <- (ifElse (isJust cached) (return (updated, cached)) from_file)
    (return (final_cache, value))

lookup :: Ord k => k -> Map k v -> IO (Maybe v)
lookup = \key map -> do
    (lru, backing_file) <- (takeLRU map)
    (final_cache, value) <- (lookupWithLock key map lru backing_file)
    (putLRU (final_cache, backing_file) map)
    (return value)
        
delete :: Ord k => k -> Map k v -> IO ()
delete = \key map -> do
    (lru, backing_file) <- (takeLRU map)
    let (updated, value) = (LRU.delete key lru)
    (ifElse (isNothing value) (unsetBackingFile key map backing_file) noop)
    (putLRU (updated, backing_file) map)

parseBackingFile :: Ord k => Show k => Show v => Int -> ([Word8] -> (k, v)) -> Handle -> IO [(Word8, k, v)]
parseBackingFile = \width deserialize handle -> do
    let flag_width = ((+) width 1) :: Int
    ptr <- (mallocArray flag_width)
    let {parseNext = do
        (hGetBuf handle ptr flag_width)
        bytes <- (peekArray flag_width ptr) :: IO [Word8]
        let flag = (List.head bytes)
        let (key, value) = (deserialize (List.tail bytes))
        (return (flag, key, value))}
    let {parseAll = \records -> do
        eof <- (hIsEOF handle)
        let {recurse = do
            record <- (parseNext)
            (parseAll ((:) record records))}
        (ifElse eof (return records) recurse)}
    records <- (parseAll [])
    (return (List.reverse records))


