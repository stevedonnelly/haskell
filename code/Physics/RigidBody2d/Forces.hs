module Physics.RigidBody2d.Forces where
import Algebra.Vector as Vector
import Data.List as List
import Geometry.Vector2d as Vector2d
import Physics.Dynamics as Dynamics
import qualified Physics.Particle as Particle
import Physics.RigidBody2d as RigidBody2d
import Prelude.Extensions as PreludeExt

linearForce = \rigid_body direction acceleration -> let
    mass = (Physics.RigidBody2d.mass rigid_body)
    position = (Physics.RigidBody2d.position rigid_body)
    in (position, Dynamics.directedForce mass (Vector2d.fromAngle direction) acceleration)

relativeLinearForce = \rigid_body direction acceleration -> let
    in (linearForce rigid_body ((+) (orientation rigid_body) direction) acceleration)

spinForces = \rigid_body acceleration -> let
    position = (Physics.RigidBody2d.position rigid_body)
    forward_axis = (Vector2d.fromAngle (orientation rigid_body))
    right_axis = (Vector2d.rotate forward_axis (toRational ((/) (Prelude.negate pi) 2)))
    forward_force = (directedForce (inertiaMoment rigid_body) forward_axis ((/) acceleration 2))
    left_force = ((Vector.subtract position right_axis), (Vector.negate forward_force))
    right_force = ((Vector.add position right_axis), forward_force)
    in [left_force, right_force]

linearFrictionForce = \rigid_body acceleration time -> let
    velocity = (Physics.RigidBody2d.velocity rigid_body)
    decceleration = (Prelude.negate (Dynamics.frictionAcceleration (Vector.length velocity) acceleration time))
    in (linearForce rigid_body (Vector2d.toAngle velocity) decceleration)

spinFrictionForces = \rigid_body acceleration time -> let
    velocity = (Physics.RigidBody2d.angularVelocity rigid_body)
    decceleration = (Prelude.negate ((*) (signum velocity) (Dynamics.frictionAcceleration (abs velocity) acceleration time)))
    in (spinForces rigid_body decceleration)

frictionForces = \body friction angular_friction timestep -> let
    forward_friction = (linearFrictionForce body friction timestep)
    spin_friction = (spinFrictionForces body angular_friction timestep)
    in ((:) forward_friction spin_friction)

unlimitedLinearForces = \rigid_body max_velocity forces -> let
    velocity = (Physics.RigidBody2d.velocity rigid_body)
    withinMax = \(position, force) -> (isUnlimitedForce velocity max_velocity force)
    in (List.filter withinMax forces)

unlimitedSpinForces = \rigid_body max_velocity forces -> let
    velocity = (Physics.RigidBody2d.angularVelocity rigid_body)
    withinMax = \(position, force) -> let
        less_than_max = ((<) (abs velocity) max_velocity)
        direction = (Vector.subtract position (Physics.RigidBody2d.position rigid_body))
        is_opposite = ((/=) (signum velocity) (signum (crossProduct direction force)))
        in ((||) less_than_max is_opposite)
    in (List.filter withinMax forces)

