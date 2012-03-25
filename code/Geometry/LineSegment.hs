
module Geometry.LineSegment where
import Algebra.Vector as V
import Data.Tuple.Extensions as TupleExt
import qualified Geometry.Line as Line
import Prelude.Extensions as PreludeExt


type LineSegment = (Vector, Vector)
endpoint0 = fst
endpoint1 = snd
setEndpoint0 = setFst
setEndpoint1 = setSnd

fromEndpoints = \a b -> (a, b)
fromPointDirection = \p direction -> (p, V.add p direction)
line = \segment -> (Line.fromPoints (endpoint0 segment) (endpoint1 segment))

direction = ((.) Line.direction line)
scalarPoint = \s t -> (Line.scalarPoint (line s) t)

closestPoint = \segment point -> let
    scalar = (Line.projectionScalar (line segment) point)
    projection = (scalarPoint segment scalar)
    in (ifElse ((<) scalar 0) (endpoint0 segment) (ifElse ((>) scalar 1) (endpoint1 segment) projection))

toClosestPoint = \edge point -> (V.subtract (closestPoint edge point) point)

distanceSquaredToPoint = \edge point-> (V.lengthSquared (toClosestPoint edge point))

distanceToPoint = \edge point -> (V.length (toClosestPoint edge point))


