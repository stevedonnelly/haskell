
module Algebra.Vector where
import Control.Exception.Base
import Data.List as List
import qualified Data.List.Extensions as ListExt
import qualified Data.Map as Map
import Data.Ratio as Ratio
import Data.Ratio.Extensions as RatioExt
import Prelude.Extensions as PreludeExt


type Vector = [Rational]

fromList = id
toList = id
fromArray = ((.) Algebra.Vector.fromList Map.elems)
toArray = ((.) ListExt.toArray0 Algebra.Vector.toList)

size = List.length
sameSize = \a b -> ((==) (size a) (size b))

element = \v index -> ((!!) v index)

zero = \n -> (List.replicate n (toRational 0))
isZero = \v -> (all ((==) (0::Rational)) v)
notZero = ((.) not isZero)

map = List.map
map2 = \f a b -> let
    preconditions = (sameSize a b)
    result = (List.map (uncurry f) (List.zip a b))
    in (assert preconditions result)

add :: Vector -> Vector -> Vector
add = (map2 (+))

sum = (List.foldr add (zero 2))

subtract :: Vector -> Vector -> Vector
subtract = (map2 (-))

scale :: Rational -> Vector -> Vector
scale = \scalar v -> (Algebra.Vector.map ((*) scalar) v)

scaleTo = \magnitude v -> let
    preconditions = ((>) length 0)
    length = (Algebra.Vector.length v)
    result = (scale ((/) magnitude length) v)
    in (assert preconditions result)

scaleToOrZero = \magnitude v -> let
    length = (Algebra.Vector.length v)
    scaled = (scale ((/) magnitude length) v)
    in (ifElse ((==) length 0) v scaled)

negate = \v -> (scale (Prelude.negate 1) v)

dotProduct = \a b -> (List.sum (map2 (*) a b))

angle = \a b -> let
    dot = (dotProduct a b)
    lengths = ((*) (Algebra.Vector.length a) (Algebra.Vector.length b))
    in (toRational (acos (fromRational ((/) dot lengths))))

lengthSquared = \v -> (dotProduct v v)

length = \v -> (toRational (sqrt (fromRational (lengthSquared v))))

distanceSquared = \a b -> (lengthSquared (Algebra.Vector.subtract b a))
distance = \a b -> (Algebra.Vector.length (Algebra.Vector.subtract b a))

normalize = \v -> (scale ((/) (toRational 1) (Algebra.Vector.length v)) v)

normalizeOrZero = \v -> (ifElse ((==) (lengthSquared v) (toRational 0)) (zero (size v)) (normalize v))

projectionScalar = \base vector -> let
    base_squared = (lengthSquared base)
    in (ifElse ((==) base_squared 0) 0 ((/) (dotProduct base vector) base_squared))

projection = \base vector -> (scale (projectionScalar base vector) base)

setPrecision = \precision -> (List.map (RatioExt.setPrecision precision))
setPrecision10 = (Algebra.Vector.setPrecision ((%) 1 10000000000))


