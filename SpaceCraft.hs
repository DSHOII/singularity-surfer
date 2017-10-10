module SpaceCraft where

import           Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

spaceCraft :: GLfloat -> IO ()
spaceCraft w = renderPrimitive Triangles $ mapM_ vertex3f
    [ ( -w, w, w), ( w, w,-w), ( w,-w,-w) ]
