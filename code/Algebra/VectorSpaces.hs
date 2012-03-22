module Algebra.VectorSpaces where
import Algebra.Matrix as Matrix
import Algebra.Vector as Vector
import Data.List as List
import Data.List.Extensions as ListExt
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
        reduced = (Matrix.fromRowList ((++) (List.map subtractPivotRow previous) 
            ((:) pivot_row (List.map subtractPivotRow (tail remaining)))))
        recurse = (rowReduction reduced ((+) row 1) ((+) column 1))
        in (ifElse at_end matrix (ifElse no_pivot skip_column recurse))
    in (rowReduction matrix 0 0)

backSubstitution = \matrix output -> let
    (rows, columns) = (Matrix.size matrix)
    freeColumns = \(free_columns, pivot) column -> let
        is_pivot = ((&&) ((/=) pivot rows) ((==) (Vector.element column pivot) 1))
        updated_columns = (ifElse (Vector.notZero column) ((:) column free_columns) free_columns)
        in (ifElse is_pivot (free_columns, (+) pivot 1) (updated_columns, pivot))
    (free_columns, pivot_rows) = (List.foldl freeColumns ([], 0) (Matrix.toColumnList matrix))
    is_consistent = (all ((==) 0) (List.drop pivot_rows (Vector.toList output)))
    in (ifElse is_consistent ((:) output (List.reverse free_columns)) [])

solveLinearSystem = \matrix output -> let
    (rows, columns) = (Matrix.size matrix)
    merged = (ListExt.map2 (++) (Matrix.toRowLists matrix) (List.map (\x -> [x]) (Vector.toList output)))
    reduction = (rowReduction (Matrix.fromRowLists merged))
    (reduced_matrix, reduced_output) = (List.splitAt columns (Matrix.toRowLists reduction))
    in (backSubstitution (Matrix.fromRowLists reduced_matrix) (Vector.fromList (List.map head reduced_output)))


    
