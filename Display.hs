module Display (idle, display) where

import           Control.Monad
import           Data.IORef
import           Graphics.UI.GLUT
import           Points
import           SpaceCraft

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    (x',y') <- get pos
    translate $ Vector3 x' y' 0
    preservingMatrix $ do
        a <- get angle
        rotate a $ Vector3 0 0 1
        rotate a $ Vector3 0 0.1 1
        scale 0.7 0.7 (0.7::GLfloat)
        forM_ (points 1) $ \(x,y,z) -> preservingMatrix $ do
            color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
            translate $ Vector3 x y z
            spaceCraft 0.1
            -- color $ Color3 (0::GLfloat) 0 0
            -- cubeFrame 0.1
    swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
    d <- get delta
    angle $~! (+ d)
    postRedisplay Nothing
