
module Geometry.Line where
import Algebra.Vector as V
import Algebra.Matrix as M
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt


type Line = (Vector, Vector)
point0 = fst
point1 = snd
setPoint0 = setFst
setPoint1 = setSnd

direction = \line -> (V.subtract (point1 line) (point0 line))

projectionScalar = \line point -> let
    in (V.projectionScalar (direction line) (V.subtract point (point0 line)))

closestPoint = \line point -> let
    scalar = (Geometry.Line.projectionScalar line point)
    in (V.add (point0 line) (V.scale scalar (direction line)))

toClosestPoint = \line point -> (V.subtract (closestPoint line point) point)

distanceSquaredToPoint = \edge point-> (V.lengthSquared (toClosestPoint edge point))

distanceToPoint = \edge point -> (V.length (toClosestPoint edge point))

intersectionScalars = \line0 line1 -> let
    directions0 = (V.toList (direction line0))
    directions1 = (V.toList (V.negate (directions line1)))
    columns = (List.map V.fromList [directions0, directions1])
    matrix = (M.transpose (M.fromList columns))
    output = (V.subtract (point0 line1) (point0 line0))
    in (M.solve matrix output)

