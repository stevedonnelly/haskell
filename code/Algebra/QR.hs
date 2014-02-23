
module Algebra.QR where
import Algebra.Matrix as Matrix
import Algebra.Vector as Vector
import Data.List as List
import Data.List.Extensions as ListExt
import Prelude.Extensions as PreludeExt


rowReduction = \matrix -> let
    maximizeRow = \remaining_rows index -> let
        selectRow = \previous current -> (ifElse ((>) ((!!) current index) ((!!) previous index)) current previous)
        in (List.foldr selectRow (head remaining_rows) (tail remaining_rows))
    processDiagonal = \reduced_rows index -> let
        (previous_rows, remaining_rows) = (List.splitAt index reduced_rows)
        max_row = (maximizeRow remaining_rows index)
        current_row = (Vector.add (head remaining_rows) max_row)
        scaled_row = (Vector.scale ((/) 1 ((!!) current_row index)) current_row)
        subtractScaledRow = \row -> (Vector.subtract row (Vector.scale ((!!) row index) scaled_row))
        in ((++) (List.map subtractScaledRow previous_rows) 
            ((:) scaled_row (List.map subtractScaledRow (tail remaining_rows))))
    in (List.foldl processDiagonal matrix (range0 (uncurry min (Matrix.size matrix))))

linearSystemSolution = \transform output -> let
    (row_size, column_size) = (Matrix.size transform)
    merged_rows = (Vector.map2 (++) transform output)
    reduction = (rowReduction merged_rows)
    (reduced_transform, reduced_output) = (unzip (List.map (List.splitAt column_size) reduction))
    in reduced_output

inverse = \matrix -> (linearSystemSolution matrix (Matrix.identity (fst (Matrix.size matrix))))

leastSquaresSolution = \transform output -> let
    transposed_transform = (Matrix.transpose transform)
    projected_transform = (Matrix.multiply transposed_transform transform)
    projected_output = (Matrix.multiply transposed_transform output)
    in (linearSystemSolution projected_transform projected_output)


subtractProjections = \base vectors -> let
    scalars = (List.map (projectionScalar base) vectors)
    subtracted = (List.map (\(vector, scalar) -> (Vector.subtract vector (Vector.scale scalar base))) (zip vectors scalars))
    in (subtracted, scalars)

orthogonalDecomposition = \matrix -> let
    columns = (Matrix.transpose matrix)
    processColumn = \(bases, scalars) index -> let
        (previous, current) = (List.splitAt index bases)
        (subtracted, scalar_row) = (subtractProjections (head current) (tail current))
        result_bases = ((++) previous ((:) (head current) subtracted))
        result_scalar_row = ((++) (replicate index 0) ((:) 1 scalar_row))
        in (result_bases, ((:) result_scalar_row scalars))
    (basis, scalars) = (List.foldl processColumn (columns, []) (range0 (List.length columns)))
    in ((Matrix.transpose basis), (List.reverse scalars))

orthogonalize = ((.) fst orthogonalDecomposition)

orthonormalDecompositionOfOrthogonalDecomposition = \(basis, scalars) -> let
    columns = (Matrix.transpose basis)
    processColumn = \(column, scalar_row) -> let
        length = (Vector.length column)
        in ((Vector.scale ((/) 1 length) column), (Vector.scale length scalar_row))
    normalized = (List.map processColumn (zip columns scalars))
    (normalized_columns, normalized_scalars) = (unzip normalized)
    in ((Matrix.transpose normalized_columns), normalized_scalars)

orthonormalDecomposition = ((.) orthonormalDecompositionOfOrthogonalDecomposition orthogonalDecomposition)

orthonormalize = ((.) fst orthonormalDecomposition)


