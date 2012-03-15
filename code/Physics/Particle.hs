
module Physics.Particle where
import Control.Exception.Base
import Data.List as List
import Data.Ratio as Ratio
import Algebra.Vector as Vector
import Extensions.Prelude as PreludeExt
import Extensions.Data.Tuple as TupleExt
import Physics.Dynamics as Dynamics


type Particle = (Rational, Vector, Vector)
(mass, setMass,
 position, setPosition,
 velocity, setVelocity)
 = namedTuple3


move = \particle time -> (setPosition particle (Dynamics.move (position particle) (velocity particle) time))

applyForce = \particle force time -> let
    acceleration = (Vector.scale ((/) 1 (mass particle)) force)
    (new_position, new_velocity) = (Dynamics.accelerate (position particle) (velocity particle) acceleration time)
    in (setVelocity (setPosition particle new_position) new_velocity)

applyForces = \particle forces time -> (applyForce particle (Vector.sum forces) time)


setPrecision = \precision (mass, position, velocity) -> let
    setP = (Vector.setPrecision precision)
    in (mass, (setP position), (setP velocity))

setPrecision10 = (Physics.Particle.setPrecision ((%) 1 10000000000))


