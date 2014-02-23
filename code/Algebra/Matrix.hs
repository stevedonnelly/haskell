
module Algebra.Matrix where
import Algebra.Vector as Vector
import Control.Exception.Base
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Ratio as Ratio
import Prelude.Extensions as PreludeExt


type Matrix = [Vector]

fromRowList = id
toRowList = id
fromColumnList = ((.) Algebra.Matrix.transpose fromRowList)
toColumnList = ((.) toRowList Algebra.Matrix.transpose)
fromRowArray = ((.) fromRowList Map.elems)
toRowArray = ((.) ListExt.toArray0 toRowList)
fromRowLists = ((.) fromRowList (List.map Vector.fromList))
toRowLists = ((.) (List.map Vector.toList) toRowList)
fromRowArrays = ((.) fromRowArray (Map.map Vector.fromArray))
toRowArrays = ((.) (Map.map Vector.toArray) toRowArray)

size = \m -> let
    rows = (List.length m)
    columns = (ifElse ((>) rows 0) (Vector.size (head m)) 0)
    in (rows, columns)

row = (List.!!)
column = \m index -> (row (Algebra.Matrix.transpose m) index)
element = \m row_index column_index -> (Vector.element (row m row_index) column_index)

identity = \n -> let
    n_1 = ((-) n 1)
    processRow = \index -> let
        left = (Vector.zero index)
        right = ((:) (toRational 1) (Vector.zero ((-) n_1 index)))
        in ((++) left right)
    in (List.map processRow (range0 n))

rowMatrix = \row_vector -> [row_vector]
columnMatrix = \column_vector -> (Algebra.Matrix.transpose (rowMatrix column_vector))

add = (Vector.map2 Vector.add)

scale = \scalar -> (List.map (Vector.scale scalar))

isValidSize = \m -> let
    equal_row_size = (and (List.map (Vector.sameSize (head m)) (tail m)))
    in ((||) (List.null m) equal_row_size)

transpose = \m -> let
    preconditions = (isValidSize m)
    result = (List.transpose m)
    in (assert preconditions result)

multiply = \a b -> let
    b_columns = (Algebra.Matrix.transpose b)
    processRow = \row -> let
        processColumn = \column ->
            (dotProduct row column)
        in (List.map processColumn b_columns)
    in (List.map processRow a)

transform = \matrix column_vector -> (column (multiply matrix (columnMatrix column_vector)) 0)

setPrecision = \precision -> (List.map (Vector.setPrecision precision))
setPrecision10 = (Algebra.Matrix.setPrecision ((%) 1 10000000000))


