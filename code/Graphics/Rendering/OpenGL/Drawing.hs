module Graphics.Rendering.OpenGL.Drawing where
import Algebra.Vector as V
import Data.List as List
import Geometry.AABB as AABB
import Geometry.LineSegment as LS
import Geometry.Polygon as Polygon
import Graphics.Rendering.OpenGL
import Prelude.Extensions as PreludeExt

vectorToGL2 = \constructor vector -> let 
    floats = (List.map fromRational vector)
    in (constructor ((!!) floats 0) ((!!) floats 1))
vectorToVertex2 :: Vector -> Vertex2 GLdouble
vectorToVertex2 = (vectorToGL2 Vertex2)
vectorToVector2 :: Vector -> Vector2 GLdouble
vectorToVector2 = (vectorToGL2 Vector2)

vectorToGL3 = \constructor vector -> let 
    floats = (List.map fromRational vector)
    in (constructor ((!!) floats 0) ((!!) floats 1) ((!!) floats 2))
vectorToVertex3 :: Vector -> Vertex3 GLdouble
vectorToVertex3 = (vectorToGL3 Vertex3)
vectorToColor3 :: Vector -> Color3 GLdouble
vectorToColor3 = (vectorToGL3 Color3)
vectorToVector3 :: Vector -> Vector3 GLdouble
vectorToVector3 = (vectorToGL3 Vector3)

drawAABB2d :: AABB -> IO()
drawAABB2d = \aabb -> do
    let ([x0, y0], [x1, y1]) = aabb
    renderPrimitive Polygon $ do
        (drawVertices2d [[x0, y0], [x1, y0], [x1, y1], [x0, y1]])

drawPoint2d :: Vector -> IO()
drawPoint2d = \point -> preservingMatrix $ do
    renderPrimitive Points $ do
        (vertex (vectorToVertex2 point))

drawLineSegment2d :: LineSegment -> IO()
drawLineSegment2d = \line_segment -> preservingMatrix $ do
    renderPrimitive Lines $ do
        (vertex (vectorToVertex2 (endpoint0 line_segment)))
        (vertex (vectorToVertex2 (endpoint1 line_segment)))

drawVertices2d :: [Vector] -> IO()
drawVertices2d = \points -> do (mapM_ ((.) vertex vectorToVertex2) points)

drawPolygon2d :: Polygon -> IO()
drawPolygon2d = \polygon ->  preservingMatrix $ do
    renderPrimitive Polygon $ do
        (drawVertices2d (Polygon.points polygon))

