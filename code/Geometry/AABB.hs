
module Geometry.AABB where
import Algebra.Vector as Vector
import Control.Exception.Base
import Data.List as List
import Data.Ratio as Ratio
import Data.Tuple.Extensions as TupleExt


type AABB = (Vector, Vector)
minCorner = fst
maxCorner = snd 
setMinCorner = setFst 
setMaxCorner = setSnd

center = \aabb -> (Vector.scale ((%) 1 2) (Vector.add (minCorner aabb) (maxCorner aabb)))

isValidDimensions = \aabb -> let
    same_size = ((==) (Vector.size (minCorner aabb)) (Vector.size (maxCorner aabb)))
    ordered = (and (List.map (uncurry (<)) (zip (minCorner aabb) (maxCorner aabb))))
    in ((&&) same_size ordered)

overlaps :: AABB -> AABB -> Bool
overlaps = \a b -> let
    preconditions = ((&&) (isValidDimensions a) (isValidDimensions b))
    noOverlap = \a b -> ((||) ((<) (snd a) (fst b)) ((<) (snd b) (fst a)))
    no_overlaps = (Vector.map2 noOverlap ((uncurry zip) a) ((uncurry zip) b))
    result = (not (and no_overlaps))
    in (assert preconditions result)
    

