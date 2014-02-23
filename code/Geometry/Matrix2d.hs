
module Geometry.Matrix2d where
import Data.List as List
import Algebra.Matrix as M
import Algebra.Vector as V

rotation = \radians -> let
    a = (fromRational radians)
    cosa = (toRational (cos a))
    sina = (toRational (sin a))
    result = (M.fromRowList (List.map V.fromList [[cosa, (Prelude.negate sina)], [sina, cosa]]))
    in result


