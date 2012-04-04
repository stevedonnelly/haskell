module Codec.Image.DevIL.Extensions where
import Algebra.Vector as Vector
import Codec.Image.DevIL as Devil
import Data.Array.Unboxed as UArray
import Data.List as List
import Data.Map as Map
import Data.Tuple.Extensions as TupleExt
import Data.Word as Word

imageArrayToMap :: (UArray (Int, Int, Int) Word8) -> (Map Vector Vector)
imageArrayToMap = \array -> let
    height = ((-) (List.maximum (List.map snd3 (UArray.indices array))) 1)
    toMapElement = \((row, column, channel), color) -> let
        in (Vector.fromList (List.map toRational [column, (-) height row]), Map.singleton channel color)
    channel_maps = (Map.fromListWith Map.union (List.map toMapElement (UArray.assocs array)))
    in (Map.map (\x -> (Vector.fromList (List.map toRational (Map.elems x)))) channel_maps)

readImageMap :: FilePath -> IO (Map Vector Vector)
readImageMap = \file_path -> do
    array <- (Devil.readImage file_path)
    (return (imageArrayToMap array))


