
module Geometry.LineSegment where
import Algebra.Vector as V
import Data.Tuple.Extensions as TupleExt
import Prelude.Extensions as PreludeExt


type LineSegment = (Vector, Vector)
endPoint0 = fst
endPoint1 = snd
setEndPoint0 = setFst
setEndPoint1 = setSnd

closestPoint = \(a, b) point -> let
    direction = (V.subtract b a)
    to_point = (V.subtract point a)
    scalar = (projectionScalar direction to_point)
    in (ifElse ((<) scalar 0) a (ifElse ((>) scalar 1) b (V.add a (V.scale scalar direction))))

toClosestPoint = \edge point -> (V.subtract (closestPoint edge point) point)

distanceSquaredToPoint = \edge point-> (V.lengthSquared (toClosestPoint edge point))

distanceToPoint = \edge point -> (V.length (toClosestPoint edge point))


