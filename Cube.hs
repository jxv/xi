{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cube where

import Control.Lens
import Control.Monad
import Data.Array.IO
import Data.Array.Storable
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.Exit
import Graphics.Rendering.OpenGL.Raw

import Control.Exception (catch, SomeException)
import Data.Bits ((.|.))

import qualified Graphics.UI.GLUT as GLUT
import qualified Linear as L
import qualified Xi as Xi

------------------------------------------------------------------------------------------

import Control.Applicative
import Foreign.Marshal hiding (void)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

------------------------------------------------------------------------------------------

data App = App
  { appXi :: Xi.Xi
  , appUserData :: IORef UserData
  }

data UserData = UserData
  { udProgram          :: GLuint
  , udPositionLoc      :: GLuint
  , udModelViewProjLoc :: GLint
  , udAngle            :: GLfloat
  , udVertPtr          :: Ptr GLfloat
  , udIdxPtr           :: Ptr GLuint
  , udModelViewProjMat :: Xi.Mat4
  , udProjection       :: Bool
  }

------------------------------------------------------------------------------------------

makeApp :: IO App
makeApp = do
  prog <- Xi.makeProgram "data/Cube.vert" "data/Cube.frag"
  glUseProgram prog
  --
  posLoc <- B.unsafeUseAsCString "a_position\0" (glGetAttribLocation prog)
  mvpLoc <- B.unsafeUseAsCString "u_modelViewProjMat\0" (glGetUniformLocation prog)
  --
  vptr <- mallocList cubeVerts
  iptr <- mallocList cubeIndices
  --
  ud <- newIORef (userData prog posLoc mvpLoc vptr iptr)
  return $ App xi ud
 where
  xi = Xi.makeXi GLUT.swapBuffers
  userData prog posLoc mvpLoc vptr iptr = UserData
    { udProgram = prog
    , udPositionLoc = fromIntegral posLoc
    , udModelViewProjLoc = mvpLoc
    , udAngle = 45
    , udVertPtr = vptr
    , udIdxPtr = iptr
    , udModelViewProjMat = L.eye4
    , udProjection = True
    }

------------------------------------------------------------------------------------------

main :: IO ()
main = do
  void GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [GLUT.RGBMode, GLUT.WithDepthBuffer]
  GLUT.initialWindowSize GLUT.$= GLUT.Size 640 480
  GLUT.createWindow "Xi"
  --
  app <- catch (setup >> makeApp) (\ex -> print  (ex :: SomeException) >> exitFailure)
  --
  GLUT.reshapeCallback GLUT.$= Just (reshape app)
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse app)
  GLUT.motionCallback GLUT.$= Just (motion app)
  GLUT.displayCallback GLUT.$= (display app)
  GLUT.addTimerCallback 20 (timer app)
  --
  GLUT.mainLoop
  --
  ud <- readIORef (appUserData app)
  free (udVertPtr ud)
  free (udIdxPtr ud)
  glDeleteProgram (udProgram ud)

------------------------------------------------------------------------------------------

setup :: IO ()
setup = do
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  --
  glClearColor 0.2 0.2 0.2 1.0
  --
  glEnable gl_CULL_FACE
  glCullFace gl_BACK

------------------------------------------------------------------------------------------

timer :: App -> GLUT.TimerCallback
timer app@App{..} = do
  (GLUT.Size width height) <- GLUT.get GLUT.windowSize
  modifyIORef appUserData $ \ud@UserData{..} ->
    let angle  = udAngle + 40 * (dt / 1000)
        angle' = (if angle >= 360 then subtract 360 else id) angle

        camera = Xi.Camera (Xi.Perspective 60)
                           (fromIntegral width / fromIntegral height)
                           1
                           20
                           (L.V3 0 0 0)
                           (L.V3 0 0 0)
                           (L.V3 0 0 0)
        camera' = if udProjection then camera else camera { Xi.cameraProjection = Xi.Ortho }
        projection = Xi.cameraProjectionMat camera'
        mvp = (Xi.rotate (Xi.translate L.eye4 0 0 (-2)) angle' 1 0 1) L.!*! projection
    in ud { udAngle = angle'
          , udModelViewProjMat = mvp
          }
  --
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback dt (timer app)
 where
  dt :: Num a => a
  dt = 20

display :: App -> GLUT.DisplayCallback
display App{..} = do
  UserData{..} <- readIORef appUserData
  --
  (GLUT.Size width height) <- GLUT.get GLUT.windowSize
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  --
  glUseProgram udProgram
  glVertexAttribPointer udPositionLoc 3 gl_FLOAT 0 (fromIntegral $ 3 * sizeOf (undefined :: GLfloat)) udVertPtr
  glEnableVertexAttribArray udPositionLoc
  --
  with udModelViewProjMat (glUniformMatrix4fv udModelViewProjLoc 1 0 . castPtr)
  glDrawElements gl_TRIANGLES (36) gl_UNSIGNED_INT udIdxPtr
  --
  glFlush
  GLUT.swapBuffers

reshape :: App -> GLUT.ReshapeCallback
reshape App{..} size = do
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)

keyboardMouse :: App -> GLUT.KeyboardMouseCallback
keyboardMouse App{..} key keyState modifiers pos = do
  case (key, keyState) of
    (GLUT.Char 'p', GLUT.Up) ->
      modifyIORef appUserData (\ud -> ud { udProjection = not (udProjection ud)})
    _ -> return ()

motion :: App -> GLUT.MotionCallback
motion App{..} pos = do
  return ()

------------------------------------------------------------------------------------------

cubeVerts :: [GLfloat]
cubeVerts =
  [ -0.5, -0.5, -0.5
  , -0.5, -0.5,  0.5
  , 0.5, -0.5,  0.5
  , 0.5, -0.5, -0.5
  , -0.5,  0.5, -0.5
  , -0.5,  0.5,  0.5
  , 0.5,  0.5,  0.5
  , 0.5,  0.5, -0.5
  , -0.5, -0.5, -0.5
  , -0.5,  0.5, -0.5
  , 0.5,  0.5, -0.5
  , 0.5, -0.5, -0.5
  , -0.5, -0.5, 0.5
  , -0.5,  0.5, 0.5
  , 0.5,  0.5, 0.5 
  , 0.5, -0.5, 0.5
  , -0.5, -0.5, -0.5
  , -0.5, -0.5,  0.5
  , -0.5,  0.5,  0.5
  , -0.5,  0.5, -0.5
  , 0.5, -0.5, -0.5
  , 0.5, -0.5,  0.5
  , 0.5,  0.5,  0.5
  , 0.5,  0.5, -0.5
  ]

cubeNormals :: [GLfloat]
cubeNormals =
  [ 0.0, -1.0, 0.0
  , 0.0, -1.0, 0.0
  , 0.0, -1.0, 0.0
  , 0.0, -1.0, 0.0
  , 0.0, 1.0, 0.0
  , 0.0, 1.0, 0.0
  , 0.0, 1.0, 0.0
  , 0.0, 1.0, 0.0
  , 0.0, 0.0, -1.0
  , 0.0, 0.0, -1.0
  , 0.0, 0.0, -1.0
  , 0.0, 0.0, -1.0
  , 0.0, 0.0, 1.0
  , 0.0, 0.0, 1.0
  , 0.0, 0.0, 1.0
  , 0.0, 0.0, 1.0
  , -1.0, 0.0, 0.0
  , -1.0, 0.0, 0.0
  , -1.0, 0.0, 0.0
  , -1.0, 0.0, 0.0
  , 1.0, 0.0, 0.0
  , 1.0, 0.0, 0.0
  , 1.0, 0.0, 0.0
  , 1.0, 0.0, 0.0
  ]

cubeTex :: [GLfloat]
cubeTex =
  [ 0.0, 0.0
  , 0.0, 1.0
  , 1.0, 1.0
  , 1.0, 0.0
  , 1.0, 0.0
  , 1.0, 1.0
  , 0.0, 1.0
  , 0.0, 0.0
  , 0.0, 0.0
  , 0.0, 1.0
  , 1.0, 1.0
  , 1.0, 0.0
  , 0.0, 0.0
  , 0.0, 1.0
  , 1.0, 1.0
  , 1.0, 0.0
  , 0.0, 0.0
  , 0.0, 1.0
  , 1.0, 1.0
  , 1.0, 0.0
  , 0.0, 0.0
  , 0.0, 1.0
  , 1.0, 1.0
  , 1.0, 0.0
  ]

cubeIndices :: [GLuint]
cubeIndices =
  [ 0, 2, 1
  , 0, 3, 2
  , 4, 5, 6
  , 4, 6, 7
  , 8, 9, 10
  , 8, 10, 11
  , 12, 15, 14
  , 12, 14, 13
  , 16, 17, 18
  , 16, 18, 19
  , 20, 23, 22
  , 20, 22, 21
  ]

mallocList :: Storable a => [a] -> IO (Ptr a)
mallocList list = do
  ptr <- mallocArray (length list)
  pokeArray ptr list
  return ptr

