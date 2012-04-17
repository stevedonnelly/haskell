module Geometry.SpatialHash where
import Algebra.Vector as V
import Data.List as List
import Data.List.Extensions as ListExt
import Data.Ratio as Ratio
import Data.Ratio.Extensions as RatioExt
import Geometry.AABB as AABB

type SpatialHash a = (Map Vector a, Vector, Vector)
positionMap = fst3
origin = snd3
binDimensions = third3
setPositionMap = setFst3
setOrigin = setSnd3
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

centeredPointHash = \spatial_map point -> let
    in (pointHash (binDimension spatial_hash) (V.subtract position (origin spatial_hash)))

centeredAABBHash = \spatial_map aabb -> let
    centered_aabb = (AABB.translate aabb (V.negate (origin spatial_map)))
    in (aabbHash (binDimensions spatial_hash) centered_aabb)

insert = \position bin spatial_hash -> let
    hash = (centeredPointHash spatial_hash position)
    in (setPositionMap spatial_map (Map.insert hash bin (positionMap spatial_hash)))
    
lookupPoint = \position spatial_hash -> let
    in ((!) (positionMap spatial_hash) (centeredPointHash spatial_hash position))

lookupAABB = \aabb spatial_hash -> let
    hashes = (centedAABBHash spatial_hash aabb)
    in (List.map (flip (!) (positionMap spatial_hash)) hashes)



