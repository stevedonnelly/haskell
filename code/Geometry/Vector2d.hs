
module Geometry.Vector2d where
import Algebra.Vector as Vector
import Prelude.Extensions as PreludeExt


crossProduct :: Vector -> Vector -> Rational
crossProduct = \a b -> let
    e = Vector.element
    in ((-) ((*) (e a 0) (e b 1)) ((*) (e a 1) (e b 0)))

quadrant :: Vector -> Int
quadrant = \vector -> let
    (x, y) = (Vector.element vector 0, Vector.element vector 1)
    case_list = [((&&) ((>) x 0) ((>=) y 0), 0),
        ((&&) ((>=) 0 x) ((>) y 0), 1),
        ((&&) ((>) 0 x) ((>=) 0 y), 2),
        ((&&) ((>=) x 0) ((>) 0 y), 3)]
    in (cases case_list (-1))

fromAngle = \angle -> let
    a = (fromRational angle)
    cosa = (toRational (cos a))
    sina = (toRational (sin a))
    in Vector.fromList [cosa, sina]

toAngle = \v -> let
    x = (fromRational (Vector.element v 0))
    y = (fromRational (Vector.element v 1))
    in (toRational (atan2 y x))

angle = \v0 v1 -> let
    in (normalizeAngle0 ((-) (toAngle v1) (toAngle v0)))

positiveAngle = \v0 v1 -> let
    angle = (Geometry.Vector2d.angle v0 v1)
    in (ifElse ((==) angle 0) ((*) (toRational pi) 2) angle)

rotate = \v angle -> (fromAngle ((+) (toAngle v) angle))

perpendicular :: Vector -> Vector
perpendicular = \v -> Vector.fromList [(Prelude.negate (Vector.element v 1)), (Vector.element v 0)]

normalizeAngle :: Rational -> Rational -> Rational
normalizeAngle = \min angle -> let
    pi2 = (toRational ((*) pi 2))
    max = ((+) min pi2)
    increment = \angle -> (ifElse ((<) angle min) (increment ((+) angle pi2)) angle)
    decrement = \angle -> (ifElse ((>) angle max) (decrement ((-) angle pi2)) angle)
    in (increment (decrement angle))

normalizeAngle0 = (normalizeAngle 0)


