{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Array.IO
import Data.Array.Storable
import Data.IORef
import Data.Monoid
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal hiding (void)
import System.IO
import System.Exit
import Graphics.Rendering.OpenGL.Raw
import Linear (_x, _y, _z, (!*!), eye4, V3(..), V4(..))

import Control.Exception (catch, SomeException)
import Data.Bits ((.|.))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Graphics.UI.GLUT as GLUT

import Xi

------------------------------------------------------------------------------------------

data App = App
  { _xi :: Xi
  , _userData :: IORef UserData
  }

data UserData = UserData
  { _projectionMat :: Mat4
  , _modelViewMat  :: Mat4
  , _camera        :: Camera
  , _cube          :: Cube
  , _cubeRes       :: CubeRes
  }

data Cube = Cube
  { _cubeAngle :: F
  , _cubePos   :: Vec3
  , _cubeScale :: F
  }

data CubeRes = CubeRes
  { _cubeResProgram :: GLuint
  , _cubeResPosLoc  :: GLuint
  , _cubeResMvpLoc  :: GLint
  , _cubeResVertPtr :: Ptr GLfloat
  , _cubeResIdxPtr  :: Ptr GLuint
  }

makeLenses ''App
makeLenses ''UserData
makeLenses ''Cube
makeLenses ''CubeRes

------------------------------------------------------------------------------------------

cubeVertSrc, cubeFragSrc :: BS.ByteString
cubeVertSrc =  BS.unlines [
  "uniform mat4 u_modelViewProjMat;",
  "attribute vec4 a_position;",
  "varying vec4 v_position;",
  "void main() {",
  "  gl_Position = u_modelViewProjMat * a_position;",
  "  v_position = a_position;",
  "}"]
cubeFragSrc = BS.unlines [
  "varying vec4 v_position;",
  "void main() {",
  "  vec4 color = abs(v_position * 2.0);",
  "  color.w = 1.0;",
  "  gl_FragColor = color;",
  "}"]


makeApp :: IO App
makeApp = do
  prog <- makeProgramFromSources cubeVertSrc cubeFragSrc
  glUseProgram prog
  posLoc <- BS.unsafeUseAsCString "a_position\0" (glGetAttribLocation prog)
  mvpLoc <- BS.unsafeUseAsCString "u_modelViewProjMat\0" (glGetUniformLocation prog)
  vptr <- mallocList cubeVerts
  iptr <- mallocList cubeIndices
  ud <- newIORef (userData (fromIntegral prog) (fromIntegral posLoc) mvpLoc vptr iptr)
  return $ App xi ud
 where
  xi = makeXi GLUT.swapBuffers
  userData prog posLoc mvpLoc vptr iptr = UserData
    { _projectionMat = eye4
    , _modelViewMat = eye4
    , _camera = defaultPerspectiveCamera
    , _cube = Cube
        { _cubeAngle = 45
        , _cubePos = V3 0 0 (-2)
        , _cubeScale = 1
        }
    , _cubeRes = CubeRes
        { _cubeResProgram = prog
        , _cubeResPosLoc = posLoc
        , _cubeResMvpLoc = mvpLoc
        , _cubeResVertPtr = vptr
        , _cubeResIdxPtr = iptr 
        }
    }

main :: IO ()
main = do
  void GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [GLUT.RGBMode, GLUT.WithDepthBuffer]
  GLUT.initialWindowSize GLUT.$= GLUT.Size 640 480
  GLUT.createWindow "Xi"
  app <- catch (setup >> makeApp) (\ex -> print  (ex :: SomeException) >> exitFailure)
  GLUT.reshapeCallback GLUT.$= Just (reshape app)
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse app)
  GLUT.displayCallback GLUT.$= (display app)
  timer app 20
  GLUT.mainLoop
  ud <- readIORef (app ^. userData)
  free (ud ^. cubeRes ^. cubeResVertPtr)
  free (ud ^. cubeRes ^. cubeResIdxPtr)
  glDeleteProgram (ud ^. cubeRes ^. cubeResProgram)

setup :: IO ()
setup = do
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glClearColor 0.2 0.2 0.2 1.0
  glEnable gl_CULL_FACE
  glCullFace gl_BACK

timer :: App -> Int -> IO ()
timer app dt = do
  modifyIORef (app ^. userData) $ \ud ->
    let angle  = (ud ^. cube ^. cubeAngle) + 40 * (fromIntegral dt / 1000)
        angle' = (if angle >= 360 then subtract 360 else id) angle
        cameraMat = lookAt (V3 0 0 2) (V3 0 0 (-1)) (V3 0 1 0)
        pMat = cameraProjectionMat (ud ^. camera)
        cpos = ud ^. camera ^. camPos
        cscale = ud ^. camera ^. camScale
        mvMat = scaleMat (1 * cscale) (1 * cscale) (1 * cscale) $
                rotateMat angle' 1 0 1 $
                translateMat (0 - (cpos^._x)) (0 - (cpos^._y)) (-2 - (cpos^._z)) eye4
    in ud & (cube %~ cubeAngle .~ angle')
          . (modelViewMat .~ mvMat)
          . (projectionMat .~ pMat)
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback dt (timer app dt)

display :: App -> IO ()
display app = do
  (GLUT.Size width height) <- GLUT.get GLUT.windowSize
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
  ud <- readIORef (app ^. userData)
  draw (ud ^. cube) (ud  ^. cubeRes) (ud ^. modelViewMat !*! ud ^. projectionMat)
  glFlush
  GLUT.swapBuffers
 where
  draw c cc mvp = do
    glUseProgram (cc^.cubeResProgram)
    let vertSize = fromIntegral $ 3 * sizeOf (undefined :: GLfloat)
    glVertexAttribPointer (cc ^. cubeResPosLoc) 3 gl_FLOAT 0 vertSize (cc ^. cubeResVertPtr)
    glEnableVertexAttribArray (cc ^. cubeResPosLoc)
    with mvp (\ptr -> glUniformMatrix4fv (cc ^. cubeResMvpLoc) 1 0 (castPtr ptr))
    glDrawElements gl_TRIANGLES (36) gl_UNSIGNED_INT (cc ^. cubeResIdxPtr)

reshape :: App -> GLUT.Size -> IO ()
reshape app size@(GLUT.Size w h) = do
  modifyIORef (app^.userData) $ camera %~ set camAspectRatio aspect
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)
 where
  aspect = fromIntegral w / fromIntegral h

keyboardMouse :: App -> GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
keyboardMouse App{..} key keyState modifiers pos = do
  case (key, keyState) of
    (GLUT.Char 'p', GLUT.Up) ->
      modifyIORef _userData $ \ud ->
        let isOrtho = ud ^. camera ^. camProjection == Ortho
        in ud & camera .~ (ud^.camera & camProjection .~
                            (if isOrtho then Perspective 60 else Ortho))
    (GLUT.SpecialKey GLUT.KeyUp, GLUT.Down)    -> move 0        0 (-0.005)
    (GLUT.SpecialKey GLUT.KeyDown, GLUT.Down)  -> move 0        0 0.005
    (GLUT.SpecialKey GLUT.KeyLeft, GLUT.Down)  -> move (-0.005) 0 0
    (GLUT.SpecialKey GLUT.KeyRight, GLUT.Down) -> move 0.005    0 0
    (GLUT.Char 'f', GLUT.Down) -> scale (-0.005)
    (GLUT.Char 'j', GLUT.Down) -> scale 0.005
    _ -> return ()
 where
  move x y z = modifyIORef _userData $ \ud ->
    ud & camera %~ camPos %~ over _x (+x) . over _y (+y) . over _z (+z)
  scale z = modifyIORef _userData $ \ud ->
    ud & camera %~ over camScale (\s -> if s + z < 0 then s else s + z)

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

