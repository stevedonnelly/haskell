
module Geometry.Vector3d where
import Algebra.Vector as Vector


crossProduct :: Vector -> Vector -> Vector
crossProduct = \a b -> let
    e = Vector.element
    in Vector.fromList
        [((-) ((*) (e a 1) (e b 2)) ((*) (e a 2) (e b 1))),
         ((-) ((*) (e a 2) (e b 0)) ((*) (e a 0) (e b 2))),
         ((-) ((*) (e a 0) (e b 1)) ((*) (e a 1) (e b 0)))]


