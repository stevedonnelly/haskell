
module Geometry.Line where
import Algebra.Vector as V
import Algebra.VectorSpaces as VS
import Algebra.Matrix as M
import Data.List as List
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt


type Line = (Vector, Vector)
point0 = first2
point1 = second2
setPoint0 = setFirst2
setPoint1 = setSecond2

fromPoints = \a b -> (a, b)
fromPointDirection = \p direction -> (p, V.add p direction)

direction = \line -> (V.subtract (point1 line) (point0 line))

scalarPoint = \line scalar -> (V.add (point0 line) (V.scale scalar (direction line)))

projectionScalar = \line point -> let
    in (V.projectionScalar (direction line) (V.subtract point (point0 line)))

closestPoint = \line point -> let
    in (scalarPoint line (Geometry.Line.projectionScalar line point))

toClosestPointWith = \closestPoint line point -> (V.subtract (closestPoint line point) point)

distanceSquaredToPointWith = \toClosestPoint edge point-> (V.lengthSquared (toClosestPoint edge point))

distanceToPointWith = \toClosestPoint edge point -> (V.length (toClosestPoint edge point))

toClosestPoint = (toClosestPointWith closestPoint)
distanceSquaredToPoint = (distanceSquaredToPointWith toClosestPoint)
distanceToPoint = (distanceToPointWith toClosestPoint)

intersectionScalars = \line0 line1 -> let
    columns = [direction line0, V.negate (direction line1)]
    matrix = (M.fromColumnList columns)
    output = (V.subtract (point0 line1) (point0 line0))
    in (VS.linearSystemSolution matrix output)

intersection = \line0 line1 -> let
    scalars = (intersectionScalars line0 line1)
    intersection = (scalarPoint line0 (V.element (head scalars) 0))
    length = (List.length scalars)
    in (ifElse ((==) length 0) [] (ifElse ((==) length 1) [intersection] [point0 line0, point1 line0]))
    

