module Algebra.VectorSpaces where
import Algebra.Matrix as Matrix
import Algebra.Vector as Vector
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Maybe as Maybe
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
    (row_size, column_size) = (Matrix.size matrix)
    pivot_rows = (List.takeWhile (List.any ((/=) 0)) (Matrix.toRowLists matrix))
    pivot_indices = (List.map ((.) fromJust (List.elemIndex 1)) pivot_rows)
    free_indices = ((List.\\) (ListExt.range0 column_size) pivot_indices)
    without_pivots = (List.map (ListExt.removeIndices pivot_indices) pivot_rows)
    number_of_solutions = ((+) 1 (List.length free_indices))
    solutionRow = \index -> let
        maybe_pivot_index = (List.elemIndex index pivot_indices)
        is_pivot = (isJust maybe_pivot_index)
        pivot = (fromJust maybe_pivot_index)
        pivot_row = ((:) (Vector.element output pivot) ((!!) without_pivots pivot))
        free = (fromJust (List.elemIndex index free_indices))
        free_row = (ListExt.replace (List.replicate number_of_solutions 0) ((+) free 1) (-1))
        in (ifElse is_pivot pivot_row free_row)
    solution_space = (List.map solutionRow (ListExt.range0 column_size))
    solutions = (List.map Vector.fromList (List.transpose solution_space))
    non_zero_solutions = (ifElse (notNull solutions) ((:) (head solutions) (List.filter Vector.notZero (tail solutions))) solutions)
    is_consistent = (all ((==) 0) (List.drop (List.length pivot_rows) (Vector.toList output)))
    in (ifElse is_consistent non_zero_solutions [])

linearSystemSolution = \matrix output -> let
    (rows, columns) = (Matrix.size matrix)
    merged = (ListExt.map2 (++) (Matrix.toRowLists matrix) (List.map (\x -> [x]) (Vector.toList output)))
    reduction = (rowReduction (Matrix.fromRowLists merged))
    (reduced_matrix, reduced_output) = (unzip (List.map (List.splitAt columns) (Matrix.toRowLists reduction)))
    in (backSubstitution (Matrix.fromRowLists reduced_matrix) (Vector.fromList (List.map head reduced_output)))


    
