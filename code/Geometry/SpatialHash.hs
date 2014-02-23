module Geometry.SpatialHash where
import Algebra.Vector as V
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Map as Map
import Data.Ratio as Ratio
import Data.Ratio.Extensions as RatioExt
import Data.Tuple.Extensions as TupleExt
import Geometry.AABB as AABB

type SpatialHash a = (Map Vector a, Vector, Vector)
positionMap = first3
origin = second3
binDimensions = third3
setPositionMap = setFirst3
setOrigin = setSecond3
setBinDimensions = setThird3

pointHash :: Vector -> Vector -> Vector
pointHash = \bin_dimensions point -> let
    divided = (ListExt.map2 (/) (V.toList point) (V.toList bin_dimensions))
    rounded = (List.map (RatioExt.setPrecision 1) divided)
    in (V.fromList rounded)

aabbHash :: Vector -> AABB -> [Vector]
aabbHash = \bin_dimensions aabb -> let
    min_hash = (V.toList (pointHash bin_dimensions (minCorner aabb)))
    max_hash = (V.toList (pointHash bin_dimensions (maxCorner aabb)))
    uniformSequence = \min max -> (ListExt.uniformSequence 1 min ((+) max ((%) 1 2)))
    ranges = (ListExt.map2 uniformSequence min_hash max_hash)
    hashes = (ListExt.crossProducts ranges)
    in (List.map V.fromList hashes)

centeredPointHash = \spatial_hash point -> let
    in (pointHash (binDimensions spatial_hash) (V.subtract point (origin spatial_hash)))

centeredAABBHash = \spatial_hash aabb -> let
    centered_aabb = (AABB.translate aabb (V.negate (origin spatial_hash)))
    in (aabbHash (binDimensions spatial_hash) centered_aabb)

insert = \position bin spatial_hash -> let
    hash = (centeredPointHash spatial_hash position)
    in (setPositionMap spatial_hash (Map.insert hash bin (positionMap spatial_hash)))
    
lookupPoint = \position spatial_hash -> let
    in ((!) (positionMap spatial_hash) (centeredPointHash spatial_hash position))

lookupAABB = \aabb spatial_hash -> let
    hashes = (centeredAABBHash spatial_hash aabb) :: [Vector]
    in (List.map ((!) (positionMap spatial_hash)) hashes)



