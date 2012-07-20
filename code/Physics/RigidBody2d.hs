
module Physics.RigidBody2d where
import Algebra.Vector as Vector
import Data.List as List
import Data.Ratio as Ratio
import Data.Ratio.Extensions as RatioExt
import Data.Tuple.Extensions as TupleExt
import Geometry.Vector2d as Vector2d
import Physics.Dynamics as Dynamics
import qualified Physics.Particle as Particle
import Prelude.Extensions as PreludeExt


type RigidBody2d = (Particle.Particle, Rational, Rational, Rational)
particle = first4
setParticle = setFirst4
inertiaMoment = second4
setInertiaMoment = setSecond4
orientation = third4
setOrientation = setThird4
angularVelocity = fourth4
setAngularVelocity = setFourth4
 
mass = ((.) Particle.mass particle)
position = ((.) Particle.position particle)
velocity = ((.) Particle.velocity particle)
setPosition = \body p -> (setParticle body (Particle.setPosition (particle body) p))
setVelocity = \body v -> (setParticle body (Particle.setVelocity (particle body) v))


move = \rigid_body time -> let
    new_particle = (Particle.move (particle rigid_body) time)
    new_orientation = (Dynamics.integrateVelocity (orientation rigid_body) (angularVelocity rigid_body) time)
    in (setOrientation (setParticle rigid_body new_particle) new_orientation)

applyForces = \rigid_body forces time -> let
    linear_force = (Vector.sum (List.map second forces))
    relative_forces = (Dynamics.relativePointForces (Physics.RigidBody2d.position rigid_body) forces)
    torque = (List.sum (List.map (uncurry Vector2d.crossProduct) relative_forces))
    new_particle = (Particle.applyForce (particle rigid_body) linear_force time)
    acceleration = ((/) torque (inertiaMoment rigid_body))
    velocity = (angularVelocity rigid_body)
    new_orientation = (Dynamics.integrateAcceleration (orientation rigid_body) velocity acceleration time)
    new_velocity = (Dynamics.integrateVelocity velocity acceleration time)
    in (setAngularVelocity (setOrientation (setParticle rigid_body new_particle) new_orientation) new_velocity)

linearForce = \rigid_body direction acceleration -> let
    mass = (Physics.RigidBody2d.mass rigid_body)
    position = (Physics.RigidBody2d.position rigid_body)
    in (position, directedForce mass (Vector2d.fromAngle direction) acceleration)

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


velocityAtPoint = \rigid_body point -> let
    to_point = (Vector.subtract point (position rigid_body))
    angular = (Vector.scale (angularVelocity rigid_body) (Vector2d.perpendicular to_point))
    in (Vector.add (velocity rigid_body) angular)

impulseMagnitude = \mass_0 inertia_0 contact_0 mass_1 inertia_1 contact_1 normal relative_velocity elasticity -> let
    scalar = (Prelude.negate ((+) 1 elasticity))
    numerator = ((*) scalar (dotProduct normal relative_velocity))
    masses = ((+) ((/) 1 mass_0) ((/) 1 mass_1))
    denominator_mass = ((*) (lengthSquared normal) masses)
    square = \x -> ((*) x x)
    inertiaScalar = \inertia contact -> let
        numerator = (square (dotProduct (perpendicular contact) normal))
        in ((/) numerator inertia)
    denominator_0 = (inertiaScalar inertia_0 contact_0)
    denominator_1 = (inertiaScalar inertia_1 contact_1)
    denominator = ((+) denominator_mass ((+) denominator_0 denominator_1))
    in ((/) numerator  denominator)

impulseLinearVelocity = \mass velocity collision_normal magnitude -> let
    scalar = ((/) magnitude mass)
    in (Vector.add velocity (Vector.scale scalar collision_normal))

impulseAngularVelocity = \inertia angular_velocity to_contact collision_normal magnitude -> let
    scalar = ((/) magnitude inertia)
    dot = (dotProduct (Vector2d.perpendicular to_contact) collision_normal)
    in ((+) angular_velocity ((*) scalar dot))

impulseRigidBody = \body collision_point collision_normal impulse -> let
    contact = (Vector.subtract collision_point (position body))
    new_linear = (impulseLinearVelocity (mass body) (velocity body) collision_normal impulse)
    new_angular = (impulseAngularVelocity (inertiaMoment body) (angularVelocity body) contact collision_normal impulse)
    in (setAngularVelocity (setVelocity body new_linear) new_angular)

collisionResponse = \body_0 body_1 collision_point collision_normal elasticity -> let
    mass_0 = (mass body_0)
    inertia_0 = (inertiaMoment body_0)
    contact_0 = (Vector.subtract collision_point (position body_0))
    velocity_0 = (velocityAtPoint body_0 collision_point)
    mass_1 = (mass body_1)
    inertia_1 = (inertiaMoment body_1)
    contact_1 = (Vector.subtract collision_point (position body_1))
    velocity_1 = (velocityAtPoint body_1 collision_point)
    relative_velocity = (Vector.subtract velocity_0 velocity_1)
    impulse = (impulseMagnitude mass_0 inertia_0 contact_0 mass_1 inertia_1 contact_1 collision_normal relative_velocity elasticity)
    new_body_0 = (impulseRigidBody body_0 collision_point collision_normal impulse)
    new_body_1 = (impulseRigidBody body_1 collision_point collision_normal (Prelude.negate impulse))
    in (new_body_0, new_body_1)

collisionDetection = \body_0 body_1 collision_point collision_normal elasticity -> let
    relative_velocity = (Vector.subtract (velocityAtPoint body_0 collision_point) (velocityAtPoint body_1 collision_point))
    is_collision = ((<) (dotProduct relative_velocity collision_normal) 0)
    response = (collisionResponse body_0 body_1 collision_point collision_normal elasticity)
    in (ifElse is_collision (True, response) (False, (body_0, body_1)))


circleInertiaMoment :: (Fractional r) => r -> r -> r
circleInertiaMoment = \mass radius -> ((/) ((*) mass ((*) radius radius)) 4)

rectangleInertiaMoment :: (Fractional r) => r -> r -> r -> r
rectangleInertiaMoment = \mass width height -> ((/) ((*) mass ((+) ((*) width width) ((*) height height))) 12)

setPrecision = \precision (particle, inertia, orientation, angular_velocity) -> let
    rp = (RatioExt.setPrecision precision)
    in ((Particle.setPrecision precision particle), inertia, (rp orientation), (rp angular_velocity))

setPrecision10 = (Physics.RigidBody2d.setPrecision ((%) 1 10000000000))


