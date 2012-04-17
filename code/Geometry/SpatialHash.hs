module Geometry.SpatialHash where
import Algebra.Vector as V
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Ratio as Ratio
import Data.Ratio.Extensions as RatioExt
import Geometry.AABB as AABB

hashPoint :: Vector -> Vector -> Vector
hashPoint = \bin_dimensions point -> let
    divided = (ListExt.map2 (/) (V.toList point) (V.toList bin_dimensions))
    rounded = (List.map (RatioExt.setPrecision 1) divided)
    in (V.fromList rounded)

hashAABB :: Vector -> AABB -> [Vector]
hashAABB = \bin_dimensions aabb -> let
    min_hash = (V.toList (hashPoint bin_dimensions (minCorner aabb)))
    max_hash = (V.toList (hashPoint bin_dimensions (maxCorner aabb)))
    uniformSequence = \min max -> (ListExt.uniformSequence 1 min ((+) max ((%) 1 2)))
    ranges = (ListExt.map2 uniformSequence min_hash max_hash)
    hashes = (ListExt.crossProducts ranges)
    in (List.map V.fromList hashes)
