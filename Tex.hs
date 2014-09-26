{-# LANGUAGE OverloadedStrings #-}

module Tex where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, catch)
import Data.Bits ((.|.))
import Data.Binary (decodeFile)
import Data.IORef
import Data.Word
import Graphics.Rendering.OpenGL.Raw
import GHC.Ptr
import Foreign.Storable
import Foreign.Marshal (alloca, free)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (mallocArray, pokeArray, newArray)
import System.IO
import System.Exit (exitFailure)
import System.Random (randoms, mkStdGen)

import qualified Graphics.UI.GLUT as GLUT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BL
import qualified Xi as Xi

import Tga


data App = App
  { programObject :: GLuint
  , positionLoc :: GLint
  , texCoordLoc :: GLint
  , baseMapLoc :: GLint
  , lightMapLoc :: GLint
  , baseMapTexId :: GLuint
  , lightMapTexId :: GLuint
  , screenWidth :: GLsizei
  , screenHeight :: GLsizei
  , vertPtr :: Ptr GLfloat
  , idxPtr :: Ptr GLushort
  }


num :: (Integral a, Num b) => a -> b
num = fromIntegral


loadTexture :: String -> IO GLuint
loadTexture fp = do
  tga <- decodeFile fp

  texId <- allocaPeek $ glGenTextures 1
  glBindTexture gl_TEXTURE_2D texId
  
  let rgb = num gl_RGB
  let width = num (tgaWidth tga)
  let height = num (tgaHeight tga)
 
  BS.unsafeUseAsCString (tgaBuffer tga)
                        (glTexImage2D gl_TEXTURE_2D 0 rgb width height 0 gl_RGB gl_UNSIGNED_BYTE)

  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (num gl_LINEAR)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (num gl_LINEAR)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S (num gl_CLAMP_TO_EDGE)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T (num gl_CLAMP_TO_EDGE)

  return texId


allocaPeek :: Storable b => (Ptr b -> IO a) -> IO b
allocaPeek fn = alloca $ \ptr -> do
  fn ptr
  peek ptr


initScreenWidth :: Int
initScreenWidth = 640


initScreenHeight :: Int
initScreenHeight = 480


createApp :: IO App
createApp = do
  programObject <- Xi.makeProgram "data/Tex.vert" "data/Tex.frag"

  positionLoc <- BS.unsafeUseAsCString "a_position\0" (glGetAttribLocation programObject)
  texCoordLoc <- BS.unsafeUseAsCString "a_texCoord\0" (glGetAttribLocation programObject)

  baseMapLoc  <- BS.unsafeUseAsCString "s_baseMap\0"  (glGetUniformLocation programObject)
  lightMapLoc <- BS.unsafeUseAsCString "s_lightMap\0" (glGetUniformLocation programObject)

  baseMapTexId  <- loadTexture "data/basemap.tga"
  lightMapTexId <- loadTexture "data/lightmap.tga"

  vertPtr <- newArray verts
  idxPtr  <- newArray idxs

  glClearColor 0.2 0.2 0.2 1

  return $ App
    { programObject = programObject
    , positionLoc = positionLoc
    , texCoordLoc = texCoordLoc
    , baseMapLoc = baseMapLoc
    , lightMapLoc = lightMapLoc
    , baseMapTexId = baseMapTexId
    , lightMapTexId = lightMapTexId
    , screenWidth = num initScreenWidth
    , screenHeight = num initScreenWidth
    , vertPtr = vertPtr
    , idxPtr = idxPtr
    }
 where
  verts =
    [ -0.5,  0.5,  0.0 :: GLfloat
    ,  0.0,  0.0
    , -0.5, -0.5,  0.0
    ,  0.0,  1.0
    ,  0.5, -0.5,  0.0
    ,  1.0,  1.0
    ,  0.5,  0.5,  0.0
    ,  1.0,  0.0
    ]
  idxs = [0, 1, 2, 0, 2, 3 :: GLushort]


destroyApp :: App -> IO ()
destroyApp app = do
  with (baseMapTexId app)  (glDeleteTextures 1)
  with (lightMapTexId app) (glDeleteTextures 1)
  glDeleteProgram (programObject app)
  free (vertPtr app)
  free (idxPtr app)


main :: IO ()
main = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [GLUT.RGBMode, GLUT.WithDepthBuffer]
  GLUT.initialWindowSize GLUT.$= GLUT.Size (num initScreenWidth) (num initScreenHeight)
  GLUT.createWindow "tex"

  app <- catch createApp (\ex -> print (ex :: SomeException) >> exitFailure)
  ref <- newIORef app

  GLUT.displayCallback GLUT.$= (display ref)
  GLUT.reshapeCallback GLUT.$= (Just $ reshape ref)
  timer 20

  GLUT.mainLoop

  destroyApp app


timer :: Int -> IO ()
timer dt = do
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback dt (timer dt)


display :: IORef App -> IO ()
display ref = do
  app <- readIORef ref

  glViewport 0 0 (screenWidth app) (screenHeight app)
  glClear gl_COLOR_BUFFER_BIT

  glUseProgram (programObject app)

  glVertexAttribPointer (num $ positionLoc app) 3 gl_FLOAT 0
    (num $ 5 * sizeOf (undefined :: GLfloat)) (vertPtr app)

  glVertexAttribPointer (num $ texCoordLoc app) 2 gl_FLOAT 0
    (num $ 5 * sizeOf (undefined :: GLfloat)) (plusPtr (vertPtr app) 12)

  glEnableVertexAttribArray (num $ positionLoc app)
  glEnableVertexAttribArray (num $ texCoordLoc app)

  glActiveTexture gl_TEXTURE0
  glBindTexture gl_TEXTURE_2D (baseMapTexId app)
  glUniform1i (baseMapLoc app) 0

  glActiveTexture gl_TEXTURE1
  glBindTexture gl_TEXTURE_2D (lightMapTexId app)
  glUniform1i (lightMapLoc app) 1

  glDrawElements gl_TRIANGLES 6 gl_UNSIGNED_SHORT (idxPtr app)

  glFlush
  GLUT.swapBuffers


reshape :: IORef App -> GLUT.Size -> IO ()
reshape ref (GLUT.Size width height) = do
  modifyIORef ref $ \app -> app
    { screenWidth = width
    , screenHeight = height
    }

