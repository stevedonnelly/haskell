
module Geometry.LineSegment where
import qualified Algebra.Vector as V
import qualified Data.List as List
import Data.Tuple.Extensions as TupleExt
import qualified Geometry.Line as Line
import Prelude.Extensions as PreludeExt


type LineSegment = (V.Vector, V.Vector)
endpoint0 = fst
endpoint1 = snd
setEndpoint0 = setFst
setEndpoint1 = setSnd

fromEndpoints = \a b -> (a, b)
fromPointDirection = \p direction -> (p, V.add p direction)
line = \segment -> (Line.fromPoints (endpoint0 segment) (endpoint1 segment))

direction = ((.) Line.direction line)
scalarPoint = \s t -> (Line.scalarPoint (line s) t)
projectionScalar = \s p -> (Line.projectionScalar (line s) p)

closestPoint = \segment point -> let
    scalar = (projectionScalar segment point)
    projection = (scalarPoint segment scalar)
    in (ifElse ((<) scalar 0) (endpoint0 segment) (ifElse ((>) scalar 1) (endpoint1 segment) projection))

toClosestPoint = (Line.toClosestPointWith closestPoint)
distanceSquaredToPoint = (Line.distanceSquaredToPointWith toClosestPoint)
distanceToPoint = (Line.distanceToPointWith toClosestPoint)

intersection = \segment0 segment1 -> let
    scalars = (Line.intersectionScalars (line segment0) (line segment1))
    length = (List.length scalars)
    in0_1 = \x -> ((&&) ((<=) 0 x) ((<=) x 1))
    is_valid = (and (List.map in0_1 (V.fromList (head scalars))))
    intersection = (scalarPoint segment0 (V.element (head scalars) 0))
    endpoints = [(projectionScalar segment0 (endpoint0 segment1), endpoint0 segment1),
        (projectionScalar segment0 (endpoint1 segment1), endpoint1 segment1),
        (projectionScalar segment1 (endpoint0 segment0), endpoint0 segment0),
        (projectionScalar segment1 (endpoint1 segment0), endpoint1 segment0)]
    inner_endpoints = (List.init (List.tail (List.sort endpoints)))
    is_valid_inner = (and (List.map ((.) in0_1 fst) inner_endpoints))
    segment_intersection = (List.nub (List.map snd inner_endpoints))
    length_1_case = (ifElse is_valid [intersection] [])
    length_2_case = (ifElse is_valid_inner segment_intersection [])
    in (ifElse ((==) length 0) [] (ifElse ((==) length 1) length_1_case length_2_case))


