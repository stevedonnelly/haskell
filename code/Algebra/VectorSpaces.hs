module Algebra.VectorSpaces where
import Algebra.Matrix as Matrix
import Algebra.Vector as Vector
import Data.List as List
import Prelude.Extensions as PreludeExt

rowReduction = \matrix -> let
    compareRows = \index a b -> (compare (Vector.element a index) (Vector.element b index))
    maximumRow = \rows index -> (List.maximumBy (compareRows index) rows)
    (rows, columns) = (Matrix.size matrix)
    rowReduction = \matrix row column -> let
        at_end = ((||) ((==) row rows) ((==) column columns))
        no_pivot = ((==) (Vector.element max_row column) 0)
        skip_column = (rowReduction matrix row ((+) column 1))
        (previous, remaining) = (List.splitAt row (Matrix.toRowList matrix))
        max_row = (maximumRow remaining column)
        current_row = (Vector.add (head remaining) max_row)
        pivot_row = (Vector.scale ((/) 1 (Vector.element current_row column)) current_row)
        subtractPivotRow = \x -> (Vector.subtract x (Vector.scale (Vector.element x column) pivot_row))
        reduced = (Matrix.fromRowList ((++) 
            (List.map subtractPivotRow previous) 
            ((:) pivot_row (List.map subtractPivotRow (tail remaining)))))
        recurse = (rowReduction reduced ((+) row 1) ((+) column 1))
        in (ifElse at_end matrix (ifElse no_pivot skip_column recurse))
    in (rowReduction matrix 0 0)



