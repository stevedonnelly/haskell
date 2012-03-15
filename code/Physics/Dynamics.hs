
module Physics.Dynamics where
import Algebra.Vector as Vector
import Control.Exception.Base
import Data.List as List
import Prelude.Extensions as PreludeExt


integrateVelocity :: Fractional r => r -> r -> r -> r
integrateVelocity = \start velocity time -> ((+) start ((*) velocity time))
integrateVelocityVector = \start velocity time -> (Vector.add start (Vector.scale time velocity))

integrateAcceleration :: Fractional r => r -> r -> r -> r -> r
integrateAcceleration = \start velocity acceleration time -> let
    tt = ((*) time time)
    in ((+) start ((+) ((*) velocity time) ((*) ((/) tt 2) acceleration)))
integrateAccelerationVector = \start velocity acceleration time -> let
    tt = ((*) time time)
    in (Vector.add start (Vector.add (Vector.scale time velocity) (Vector.scale ((/) tt 2) acceleration)))

move = integrateVelocityVector
accelerate = \position velocity acceleration time -> let
    new_position = (integrateAccelerationVector position velocity acceleration time)
    new_velocity = (integrateVelocityVector velocity acceleration time)
    in (new_position, new_velocity)

frictionAcceleration :: Rational -> Rational -> Rational -> Rational
frictionAcceleration = \velocity friction time -> let
    preconditions = ((&&) ((>=) velocity 0) ((&&) ((>=) friction 0) ((>=) time 0)))
    friction_magnitude = ((*) friction time)
    stop = ((*) ((/) 1 time) velocity)
    result = (ifElse ((==) time 0) 0 (ifElse ((<) friction_magnitude velocity) friction stop))
    in (assert preconditions result)


relativePointForces = \center point_forces -> let
    in (List.map (\(point, force) -> ((Vector.subtract point center), force)) point_forces)

directedForce = \mass direction acceleration -> let
    in (Vector.scaleToOrZero ((*) mass acceleration) direction)

frictionForce = \mass velocity friction time -> let
    decceleration = (Prelude.negate (frictionAcceleration (Vector.length velocity) friction time))
    in (directedForce mass velocity decceleration)

isUnlimitedForce = \velocity max_velocity force -> let
    less_than_max = ((<=) (Vector.length velocity) max_velocity)
    in ((||) less_than_max ((<=) (dotProduct velocity force) 0))

unlimitedForces = \velocity max_velocity forces -> let
    in (List.filter (isUnlimitedForce velocity max_velocity) forces)


