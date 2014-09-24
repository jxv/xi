{-# LANGUAGE OverloadedStrings #-}

module Tex where

import Control.Applicative ((<$>))
import Data.Bits ((.|.), shiftL, shiftR)
import Data.Binary
import Data.Binary.Get (skip)
import Data.IORef
import Data.Word
import Graphics.Rendering.OpenGL.Raw
import GHC.Ptr
import Foreign.Storable
import Foreign.Marshal (alloca, free)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import System.IO
import System.Random

import qualified Graphics.UI.GLUT as GLUT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BL
import qualified Xi as Xi


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


data Tga = Tga
  { tgaHeader :: BS.ByteString
  , tgaWidth :: Word32
  , tgaHeight :: Word32
  , tgaBuffer :: BS.ByteString
  }


data Bmp = Bmp
  { bmpDataPos :: Word32
  , bmpWidth :: Word32
  , bmpHeight :: Word32
  , bmpImageSize :: Word32
  , bmpBuffer :: BS.ByteString
  }


num :: (Integral a, Num b) => a -> b
num = fromIntegral


instance Binary Tga where
  put tga = do
    put (tgaHeader tga)
    let width = tgaWidth tga
    let height = tgaHeight tga
    let w = shiftL width 8
    let h = shiftL height 8
    let x = width - (shiftR w 8)
    let y = height - (shiftR h 8)
    putWord8 (num x)
    putWord8 (num w)
    putWord8 (num y)
    putWord8 (num h)
    put (tgaBuffer tga)
  get = do
    header <- BS.pack <$> mapM (const getWord8) [1..12]
    x <- getWord8
    w <- getWord8
    y <- getWord8
    h <- getWord8
    s <- getWord8
    _ <- getWord8
    let width = (num w) * 256 + (num x)
    let height = (num h) * 256 + (num y)
    let imgSize = (num s `div` 8) * (num width) * (num height) :: Int
    buffer <- BS.pack <$> mapM (const getWord8) [1..imgSize]
    return $ Tga
      { tgaHeader = header
      , tgaWidth = width
      , tgaHeight = height
      , tgaBuffer = buffer
      }


instance Binary Bmp where
  put bmp = do
    return ()
  get = do
    --header <- BS.pack <$> mapM (replicate 54 (const getWord8))
    skip 0x0a
    dataPos <- get
    return $ Bmp
      { bmpDataPos = dataPos
      , bmpWidth = 0
      , bmpHeight = 0
      , bmpImageSize = 0
      , bmpBuffer = ""
      }


loadTexture :: String -> IO GLuint
loadTexture fp = do
  tga <- decodeFile fp

  texId <- allocaWith $ glGenTextures 1
  glBindTexture gl_TEXTURE_2D texId

  print (tgaWidth tga)
  print (tgaHeight tga)
  
  let rgb = num gl_RGB
  let width = num (tgaWidth tga)
  let height = num (tgaHeight tga)
 
  -- datPtr <- mallocList (BS.unpack $ tgaBuffer tga)
  let dat = take (num $ 3 * tgaWidth tga * tgaHeight tga) (randoms $ mkStdGen 0) :: [Word8]
  datPtr <- mallocList dat

  glTexImage2D gl_TEXTURE_2D 0 rgb width height 0 gl_RGB gl_UNSIGNED_BYTE datPtr
  free datPtr

  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (num gl_LINEAR)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (num gl_LINEAR)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S (num gl_CLAMP_TO_EDGE)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T (num gl_CLAMP_TO_EDGE)

  return texId


allocaWith :: Storable b => (Ptr b -> IO a) -> IO b
allocaWith fn = alloca $ \ptr -> do
  fn ptr
  peek ptr


createApp :: IO App
createApp = do
  programObject <- Xi.makeProgram "data/Tex.vert" "data/Tex.frag"

  positionLoc <- BS.unsafeUseAsCString "a_position\0" $ glGetAttribLocation programObject
  texCoordLoc <- BS.unsafeUseAsCString "a_texCoord\0" $ glGetAttribLocation programObject

  baseMapLoc  <- BS.unsafeUseAsCString "s_baseMap\0" $ glGetUniformLocation programObject
  lightMapLoc <- BS.unsafeUseAsCString "s_lightMap\0" $ glGetUniformLocation programObject

  baseMapTexId  <- loadTexture "data/basemap.tga"
  lightMapTexId <- loadTexture "data/lightmap.tga"

  vertPtr <- mallocList verts
  idxPtr  <- mallocList idxs

  glClearColor 0 0 0 1

  return $ App
    { programObject = programObject
    , positionLoc = positionLoc
    , texCoordLoc = texCoordLoc
    , baseMapLoc = baseMapLoc
    , lightMapLoc = lightMapLoc
    , baseMapTexId = baseMapTexId
    , lightMapTexId = lightMapTexId
    , screenWidth = 640
    , screenHeight = 480
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


mallocList :: Storable a => [a] -> IO (Ptr a)
mallocList list = do
  ptr <- mallocArray (length list)
  pokeArray ptr list
  return ptr


destroyApp :: App -> IO ()
destroyApp app = do
  with (baseMapTexId app)  $ glDeleteTextures 1
  with (lightMapTexId app) $ glDeleteTextures 1
  glDeleteProgram (programObject app)
  free (vertPtr app)
  free (idxPtr app)


main :: IO ()
main = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [GLUT.RGBMode, GLUT.WithDepthBuffer]
  GLUT.initialWindowSize GLUT.$= GLUT.Size 640 480
  GLUT.createWindow "tex"

  app <- createApp
  ref <- newIORef app

  GLUT.displayCallback GLUT.$= (display ref)
  GLUT.reshapeCallback GLUT.$= (Just $ reshape ref)
  GLUT.addTimerCallback 20 (timer 20)

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
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

  glUseProgram (programObject app)

  glVertexAttribPointer
    (num $ positionLoc app)
    3
    gl_FLOAT
    0
    (num $ 5 * sizeOf (undefined :: GLfloat))
    (vertPtr app)

  glVertexAttribPointer
    (num $ texCoordLoc app)
    2
    gl_FLOAT
    0
    (num $ 5 * sizeOf (undefined :: GLfloat))
    (alignPtr (idxPtr app) 3)

  glEnableVertexAttribArray $ num (positionLoc app)
  glEnableVertexAttribArray $ num (texCoordLoc app)

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

 
