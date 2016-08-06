module Data.Word.Extensions where
import Data.List as List
import Data.Word as Word
import Prelude.Extensions as PreludeExt
import Unsafe.Coerce as Coerce

fromIntIterations :: Integral a => Int -> a -> [Word8]
fromIntIterations = \iterations int -> let
    fromNumIterationsRecursive = \iterations int -> let
        byte = (unsafeCoerce (mod int 256)) :: Word8
        remaining = (div int 256)
        recurse = (fromNumIterationsRecursive ((-) iterations 1) remaining)
        in (ifElse ((==) iterations 0) [] ((:) byte recurse))
    in (List.reverse (fromNumIterationsRecursive iterations int))

fromIntUntil :: Integral a => a -> [Word8]
fromIntUntil = \int -> let
    fromIntUntilRecursive = \int -> let
        byte = (unsafeCoerce (mod int 256)) :: Word8
        remaining = (div int 256)
        recurse = (fromIntUntilRecursive remaining)
        in (ifElse ((==) int 0) [] ((:) byte recurse))
    in (List.reverse (fromIntUntilRecursive int))

fromInt32 :: Int -> [Word8]
fromInt32 = (fromIntIterations 4)

fromInt64 = (fromIntIterations 8)

