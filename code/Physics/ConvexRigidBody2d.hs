module Physics.ConvexRigidBody2d where
import Algebra.Vector as V
import Data.Tuple.Extensions as TupleExt
import Geometry.Polygon as Polygon
import Physics.RigidBody2d as RB

type ConvexRigidBody2d = (RigidBody2d, Polygon)
rigidBody = first2
polygon = second2
setRigidBody = setFirst2

transformedPolygon = \polygonal_body -> let
    body = (rigidBody polygonal_body)
    center_mass = (V.zero 2)
    transform = \p -> (Polygon.transform p center_mass (RB.orientation body) (RB.position body))
    in (transform (polygon polygonal_body))

penetrationPoint = \a b -> let
    in (Polygon.convexPenetrationPoint (transformedPolygon a) (transformedPolygon b))

collisionDetection = \a b resolution_ratio elasticity -> let
    (intersecting, point, penetration) = (penetrationPoint a b)
    (colliding, a_body, b_body) = (RB.collisionDetection (rigidBody a) (rigidBody b) point penetration elasticity)
    resolved_a = (RB.translate a_body (V.scale resolution_ratio penetration))
    resolved_b = (RB.translate b_body (V.scale ((-) resolution_ratio 1) penetration))
    collision_result = (True, setRigidBody a resolved_a, setRigidBody b reoslved_b)
    in (ifElse ((&&) intersecting colliding) collision_result (False, a, b))



