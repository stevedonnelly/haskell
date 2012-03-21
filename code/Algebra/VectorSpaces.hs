module Algebra.VectorSpaces where
import Algebra.Matrix as Matrix
import Algebra.Vector as Vector
import Data.List as List

rowReduction = \matrix -> let
    compareRows = \index a b -> (compare (Vector.element a index) (Vector.element b index))
    maxRow = \rows index -> (List.maximizeBy (compareRows index) rows)
    (rows, columns) = (Matrix.size matrix)
    rowReduction = \matrix row column -> let
        remaining_rows = (List.drop row (Matrix.toRowList matrix))
        max_row = (maxRow (tail remaining_rows) column)
        zero_pivot = ((==) (Vector.element max_row column) 0)
        current_row = (Vector.add (head remaining_rows) (maxRow (tail remaining_rows) column))
        normalized_row = (Vector.scale ((/) 1 (Vector.element current_row column)) current_row)
        
