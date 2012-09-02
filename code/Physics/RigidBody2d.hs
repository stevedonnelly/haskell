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
inertiaMoment = second4
orientation = third4
angularVelocity = fourth4
setParticle = setFirst4
setOrientation = setThird4
setAngularVelocity = setFourth4
 
mass = ((.) Particle.mass particle)
position = ((.) Particle.position particle)
velocity = ((.) Particle.velocity particle)
setPosition = \body p -> (setParticle body (Particle.setPosition (particle body) p))
setVelocity = \body v -> (setParticle body (Particle.setVelocity (particle body) v))

translate = \body translation -> (setPosition body (Vector.add (position body) translation))

move = \rigid_body time -> let
    new_particle = (Particle.move (particle rigid_body) time)
    new_orientation = (Dynamics.integrateVelocity (orientation rigid_body) (angularVelocity rigid_body) time)
    in (setOrientation (setParticle rigid_body new_particle) new_orientation)

applyForce = \rigid_body point_force time -> (applyForces rigid_body [point_force] time)
    
applyForces = \rigid_body point_forces time -> let
    relative_forces = (Dynamics.relativePointForces (position rigid_body) point_forces)
    torque = (List.sum (List.map (uncurry Vector2d.crossProduct) relative_forces))
    velocity = (angularVelocity rigid_body)
    acceleration = ((/) torque (inertiaMoment rigid_body))
    new_particle = (Particle.applyForces (particle rigid_body) (List.map second2 point_forces) time)
    new_orientation = (Dynamics.integrateAcceleration (orientation rigid_body) velocity acceleration time)
    new_velocity = (Dynamics.integrateVelocity velocity acceleration time)
    in (setAngularVelocity (setOrientation (setParticle rigid_body new_particle) new_orientation) new_velocity)

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


setPrecision = \precision (particle, inertia, orientation, angular_velocity) -> let
    rp = (RatioExt.setPrecision precision)
    in ((Particle.setPrecision precision particle), inertia, (rp orientation), (rp angular_velocity))

setPrecision10 = (Physics.RigidBody2d.setPrecision ((%) 1 10000000000))


