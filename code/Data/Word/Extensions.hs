module Data.Word.Extensions where
import Data.Digits as Digits
import Data.List as List
import Data.Word as Word
import Prelude.Extensions as PreludeExt
import Unsafe.Coerce as Coerce

padList = \value length list -> let
    pad_length = ((-) length (List.length list))
    in (ifElse ((>) pad_length 0)
        ((++) (List.replicate pad_length value) list)
        (List.drop (abs pad_length) list)) 

fromInt :: Integral a => Int -> a -> [Word8]
fromInt = \bytes n -> (List.map unsafeCoerce (padList 0 bytes (Digits.digits 256 n)))

fromInt32 :: Int -> [Word8]
fromInt32 = (fromInt 4)

fromInt64 :: Int -> [Word8]
fromInt64 = (fromInt 8)

fromInteger :: Integer -> [Word8]
fromInteger = ((.) (List.map unsafeCoerce) (Digits.digits 256))

toIntegral :: Integral a => [Word8] -> a
toIntegral = ((.) (Digits.unDigits 256) (List.map unsafeCoerce))
