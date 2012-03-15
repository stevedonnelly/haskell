
module Physics.RigidBody where
import Algebra.Matrix as Matrix
import Algebra.Vector as Vector
import Extensions.Data.Tuple as TupleExt
import Physics.Particle as Particle


type RigidBody = (Particle, Matrix, Matrix, Vector)
particle = fst4
setParticle = setFst4
inertiaTensor = snd4
setInertiaTensor = setSnd4
orientation = third4
setOrientation = setThird4
angularVelocity = fourth4
setAngularVelocity = setFourth4
mass = ((.) Particle.mass particle)
position = ((.) Particle.position particle)
velocity = ((.) Particle.velocity particle)

applyForces = \rigid_body forces time -> let
    linear_force = (List.foldr Vector.add (Vector.zero 3) (List.map snd forces))
    relative_forces = (List.map (\(point, force) -> (Vector.subtract point (position rigid_body), force)) forces)
    torques = List.map (uncurry Vector.crossProduct) relative_forces)
    total_torque = (List.foldr Vector.add (Vector.zero 3) torques)
    new_particle = (Particle.applyForce (particle rigid_body) linear_force time)
    angular_acceleration = (Matrix.multiply (Matrix.inverse inertiaTensor) total_torque)
    change_in_angular_velocity = (Vector.scale time angular_acceleration)
    new_angular_velocity = (Vector.add (angularVelocity rigid_body) change_in_angular_velocity)
    change_in_orientation = (Matrix.rotation3d new_angular_velocity)
    new_orientation = (Matrix.multiply change_in_orientation (orientation rigid_body))
    in (new_particle, (inertiaTensor rigid_body), new_orientation, new_agnular_velocity)


