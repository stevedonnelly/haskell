module Geometry.Matrix3d where

rotation = \[x, y, z] angle -> let
    cosa = (toRational (cos (fromRational angle)))
    sina = (toRational (sin (fromRational angle)))
    invcosa = ((-) 1 cosa)
    in [[(+) cosa (product [x, x, invcosa]), 
         (-) (product [x, y, invcosa]) ((*) z sina),
         (+) (product [x, z, invcosa]) ((*) y sina)],
        [(+) (product [y, x, invcosa]) ((*) z sina),
         (+) cosa (product [y, y, invcosa]),
         (-) (product [y, z, invcosa]) ((*) x sina)],
        [(-) (product [z, x, invcosa]) ((*) y sina),
         (+) (product [z, y, invcosa]) ((*) x sina),
         (+) cosa (product [z, z, invcosa])]]
