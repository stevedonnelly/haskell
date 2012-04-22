module Graphics.Rendering.OpenGL.Extensions where
import Data.List as List
import Graphics.Rendering.OpenGL as OpenGL

centeredOrtho :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> IO()
centeredOrtho = \x y z width height depth -> do
    let x_radius = ((/) width 2)
    let y_radius = ((/) height 2)
    let z_radius = ((/) depth 2)
    let bounds = [(-) x x_radius, (+) x x_radius, (-) y y_radius, (+) y y_radius, (-) z z_radius, (+) z z_radius]
    let [x0, x1, y0, y1, z0, z1] = (List.map fromRational bounds)
    (OpenGL.ortho x0 x1 y0 y1 z0 z1)

originOrtho = (centeredOrtho 0 0 0)


