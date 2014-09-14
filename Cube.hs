{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

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
import qualified Linear as L

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
  { _appXi :: Xi.Xi
  , _appUserData :: IORef UserData
  }

data UserData = UserData
  { _udProgram          :: GLuint
  , _udPositionLoc      :: GLuint
  , _udModelViewProjLoc :: GLint
  , _udAngle            :: GLfloat
  , _udVertPtr          :: Ptr GLfloat
  , _udIdxPtr           :: Ptr GLuint
  , _udCameraMat        :: Xi.Mat4
  , _udProjectionMat    :: Xi.Mat4
  , _udModelViewMat     :: Xi.Mat4
  , _udCamera           :: Xi.Camera
  }

makeLenses ''App
makeLenses ''UserData

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
    { _udProgram = prog
    , _udPositionLoc = fromIntegral posLoc
    , _udModelViewProjLoc = mvpLoc
    , _udAngle = 45
    , _udVertPtr = vptr
    , _udIdxPtr = iptr
    , _udCameraMat = L.eye4
    , _udProjectionMat = L.eye4
    , _udModelViewMat = L.eye4
    , _udCamera = camera
    }
  camera = Xi.Camera
    { Xi._cameraProjection = Xi.Perspective 60
    , Xi._cameraAspectRatio = 1
    , Xi._cameraNear = 1
    , Xi._cameraFar = 20
    , Xi._cameraPosition = L.V3 0 0 0
    , Xi._cameraTarget = L.V3 0 0 0
    , Xi._cameraUp = L.V3 0 0 0
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
  ud <- readIORef (app^.appUserData)
  free (ud^.udVertPtr)
  free (ud^.udIdxPtr)
  glDeleteProgram (ud^.udProgram)

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
timer app = do
  modifyIORef (app^.appUserData) $ \ud ->
    let angle  = ud^.udAngle + 40 * (dt / 1000)
        angle' = (if angle >= 360 then subtract 360 else id) angle
        cameraMat = Xi.lookAt (L.V3 0 0 2) (L.V3 0 0 (-1)) (L.V3 0 1 0)
        projectionMat = Xi.cameraProjectionMat (ud^.udCamera)
        cpos = ud^.udCamera^.Xi.cameraPosition
        translated = Xi.translate L.eye4 (0 - (cpos^.L._x)) (0 - (cpos^.L._y)) (-2 - (cpos^.L._z))
        rotated = Xi.rotate translated angle' 1 0 1
        scaled = Xi.scale rotated 0.3 0.3 0.3
        modelViewMat = scaled
    in ud { _udAngle = angle'
          , _udModelViewMat = modelViewMat
          , _udProjectionMat = projectionMat
          , _udCameraMat = cameraMat
          }
  --
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback dt (timer app)
 where
  dt :: Num a => a
  dt = 20

display :: App -> GLUT.DisplayCallback
display app = do
  ud <- readIORef (app^.appUserData)
  --
  (GLUT.Size width height) <- GLUT.get GLUT.windowSize
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
  --
  glUseProgram (ud^.udProgram)
  let vertSize = fromIntegral $ 3 * sizeOf (undefined :: GLfloat)
  glVertexAttribPointer (ud^.udPositionLoc) 3 gl_FLOAT 0 vertSize (ud^.udVertPtr)
  glEnableVertexAttribArray (ud^.udPositionLoc)
  --
  let mvp = ud^.udModelViewMat L.!*! ud^.udProjectionMat
  with mvp (\ptr -> glUniformMatrix4fv (ud^.udModelViewProjLoc) 1 0 (castPtr ptr))
  glDrawElements gl_TRIANGLES (36) gl_UNSIGNED_INT (ud^.udIdxPtr)
  --
  glFlush
  GLUT.swapBuffers

reshape :: App -> GLUT.ReshapeCallback
reshape app size@(GLUT.Size w h) = do
  modifyIORef (app^.appUserData) $ \ud -> ud & udCamera .~ (ud^.udCamera & Xi.cameraAspectRatio .~ aspect)
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)
 where
   aspect = fromIntegral w / fromIntegral h

keyboardMouse :: App -> GLUT.KeyboardMouseCallback
keyboardMouse App{..} key keyState modifiers pos = do
  case (key, keyState) of
    (GLUT.Char 'p', GLUT.Up) ->
      modifyIORef _appUserData $ \ud ->
        let isOrtho = ud^.udCamera^.Xi.cameraProjection == Xi.Ortho
        in ud & udCamera .~ (ud^.udCamera & Xi.cameraProjection .~
                              (if isOrtho then Xi.Perspective 60 else Xi.Ortho))
    (GLUT.SpecialKey GLUT.KeyUp, GLUT.Down) -> move 0 (-0.1)
    (GLUT.SpecialKey GLUT.KeyDown, GLUT.Down) -> move 0 0.1
    (GLUT.SpecialKey GLUT.KeyLeft, GLUT.Down) -> move (-0.1) 0
    (GLUT.SpecialKey GLUT.KeyRight, GLUT.Down) -> move 0.1 0
    _ -> return ()
 where
  move x y = modifyIORef _appUserData $
    udCamera %~ Xi.cameraPosition %~ (\(L.V3 a b c) -> L.V3 (a + x) b (c + y))

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

