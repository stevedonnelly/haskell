
module Geometry.AABB where
import Algebra.Vector as V
import Control.Exception.Base
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Ratio as Ratio
import Data.Tuple.Extensions as TupleExt


type AABB = (Vector, Vector)
minCorner = first2
maxCorner = second2 
setMinCorner = setFirst2 
setMaxCorner = setSecond2

fromMinMax = \min max -> (min, max)

center = \aabb -> (V.scale ((%) 1 2) (V.add (minCorner aabb) (maxCorner aabb)))

translate = \aabb translation -> let
    in (fromMinMax (V.add (minCorner aabb) translation) (V.add (maxCorner aabb) translation))

isValidDimensions :: AABB -> Bool
isValidDimensions = \aabb -> let
    same_size = ((==) (V.size (minCorner aabb)) (V.size (maxCorner aabb)))
    ordered = (and (List.map (uncurry (<)) (zip (minCorner aabb) (maxCorner aabb))))
    in ((&&) same_size ordered)

expandBounds :: AABB -> Vector -> AABB
expandBounds = \aabb point -> let
    mins = (ListExt.map2 min (V.toList (minCorner aabb)) (V.toList point))
    maxs = (ListExt.map2 max (V.toList (maxCorner aabb)) (V.toList point))
    in (fromMinMax (V.fromList mins) (V.fromList maxs))

pointsBoundingBox :: [Vector] -> AABB
pointsBoundingBox = \points -> let
    in (List.foldl expandBounds (fromMinMax (head points) (head points)) (tail points))


