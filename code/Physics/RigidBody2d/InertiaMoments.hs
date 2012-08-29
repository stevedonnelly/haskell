module Physics.RigidBody2d.InertiaMoments where

circleInertiaMoment :: (Fractional r) => r -> r -> r
circleInertiaMoment = \mass radius -> ((/) ((*) mass ((*) radius radius)) 4)

rectangleInertiaMoment :: (Fractional r) => r -> r -> r -> r
rectangleInertiaMoment = \mass width height -> ((/) ((*) mass ((+) ((*) width width) ((*) height height))) 12)


