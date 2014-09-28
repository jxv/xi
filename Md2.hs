{-# LANGUAGE RecordWildCards  #-} 
{-# LANGUAGE DeriveGeneric  #-} 
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}


module Md2 where


import Control.Applicative
import Data.Bits
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int
import GHC.Generics (Generic)
import Foreign.Storable (Storable(..), sizeOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Graphics.Rendering.OpenGL.Raw (GLuint)


data Md2Header = Md2Header
  { md2HeaderMagic :: Word32
  , md2HeaderVersion :: Word32
  , md2HeaderSkinWidth :: Word32
  , md2HeaderSkinHeight :: Word32
  , md2HeaderFramesize :: Word32
  , md2HeaderNumSkins :: Word32
  , md2HeaderNumXyz :: Word32
  , md2HeaderNumSt :: Word32
  , md2HeaderNumTris :: Word32
  , md2HeaderNumGlcmds :: Word32
  , md2HeaderNumFrames :: Word32
  , md2HeaderOfsSkins :: Word32
  , md2HeaderOfsSt :: Word32
  , md2HeaderOfsTris :: Word32
  , md2HeaderOfsFrames :: Word32
  , md2HeaderOfsGlcmds :: Word32
  , md2HeaderOfsEnd :: Word32
  } deriving (Show)


data TriVert = TriVert
  { triVerts :: (Word8, Word8, Word8)
  , triVertLightNormIdx :: Word8
  } deriving (Generic, Show)


data Frame = Frame
  { frameScale :: (Float, Float, Float)
  , frameTranslate :: (Float, Float, Float)
  , frameName :: BS.ByteString
  , frameTriVerts :: [TriVert]
  } deriving (Show)


data Tri = Tri
  { triVertIdx :: (Word16, Word16, Word16)
  , triTexIdx :: (Word16, Word16, Word16)
  } deriving (Show)


type TexCoord = (Word16, Word16)


data GLCmd = GLCmd
  { glCmdS :: Float
  , glCmdT :: Float
  , glCmdVertIdx :: Word32
  } deriving (Show)


data Md2 = Md2
  { md2Header :: Md2Header
  , md2Skins :: [BS.ByteString]
  , md2Frames :: [Frame]
  , md2Tris :: [Tri]
  , md2St :: [TexCoord]
  , md2GLCmd :: [GLCmd]
  } deriving (Show)


instance Binary Md2Header where
  put Md2Header{..} = do
    putWord32le md2HeaderMagic
    putWord32le md2HeaderVersion
    putWord32le md2HeaderSkinHeight
    putWord32le md2HeaderFramesize
    putWord32le md2HeaderNumSkins
    putWord32le md2HeaderNumXyz
    putWord32le md2HeaderNumSt
    putWord32le md2HeaderNumTris
    putWord32le md2HeaderNumGlcmds
    putWord32le md2HeaderNumFrames
    putWord32le md2HeaderOfsSkins
    putWord32le md2HeaderOfsSt
    putWord32le md2HeaderOfsTris
    putWord32le md2HeaderOfsFrames
    putWord32le md2HeaderOfsGlcmds
    putWord32le md2HeaderOfsEnd
  get = Md2Header
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le


instance Binary TriVert


putFrame :: Frame -> Put
putFrame Frame{frameScale = (sx,sy,sz), frameTranslate = (tx,ty,tz), ..} = do
  putFloat32le sx
  putFloat32le sy
  putFloat32le sz
  putFloat32le tx
  putFloat32le ty
  putFloat32le tz
  put frameName
  put frameTriVerts


getFrame :: Int -> Get Frame
getFrame numVert = do
  scale <- (,,) <$> getFloat32le <*> getFloat32le <*> getFloat32le
  translate <- (,,) <$> getFloat32le <*> getFloat32le <*> getFloat32le
  name <- getByteString 16
  verts <- sequence (replicate numVert get)
  return $ Frame
    { frameScale = scale
    , frameTranslate = translate
    , frameName = name
    , frameTriVerts = verts
    }


instance Binary Tri where
  put (Tri (vx,vy,vz) (tx,ty,tz)) = do
    putWord16le vx
    putWord16le vy
    putWord16le vz
    putWord16le tx
    putWord16le ty
    putWord16le tz
  get = Tri <$> g <*> g
   where g = (,,) <$> getWord16le <*> getWord16le <*> getWord16le


instance Binary GLCmd where
  put GLCmd{..} = do
    putFloat32le glCmdS
    putFloat32le glCmdT
    putWord32le glCmdVertIdx
  get = GLCmd <$> getFloat32le <*> getFloat32le <*> getWord32le


instance Binary Md2 where
  put Md2{..} = do
    return ()
  get = do
    header@Md2Header{..} <- get
    buffer <- getLazyByteString (fromIntegral md2HeaderOfsEnd - 68)

    let getBuf n o s = BL.take (fromIntegral n * fromIntegral s)
                     . BL.drop (fromIntegral o)
                     $ buffer

    let readBuf n g o s = runGet (sequence $ replicate (fromIntegral n) g) (getBuf n (o - 68) s)

    let skins = readBuf md2HeaderNumSkins (getByteString 64) md2HeaderOfsSkins 64
    let tris = readBuf md2HeaderNumTris get md2HeaderOfsTris (size16 * 6)
    let texCoords = readBuf md2HeaderNumSt ((,) <$> getWord16le <*> getWord16le) md2HeaderOfsSt (size16 * 2)
    let frames = readBuf md2HeaderNumFrames (getFrame $ fromIntegral md2HeaderNumXyz) md2HeaderOfsFrames md2HeaderFramesize
    let glCmds = readBuf (md2HeaderNumGlcmds `div` 3) get md2HeaderOfsGlcmds (size32 * 3)

    return $ Md2
      { md2Header = header
      , md2Skins = skins
      , md2Frames = frames
      , md2Tris = tris
      , md2St = texCoords
      , md2GLCmd = glCmds
      }
   where
    size16 = sizeOf (undefined :: Word16)
    size32 = sizeOf (undefined :: Word32)


------------------------------------------------------------------------------------------


data Anim = Anim
  { animFirstFrame :: Int
  , animLastFrame :: Int
  , animFps :: Int
  }


data AnimState = AnimState
  { animStateStartFrame :: Int
  , animStateEndFrame :: Int
  , animStateFps :: Int
  , animStateCurrTime :: Float
  , animStateOldTime :: Float
  , animStateInterpol :: Float
  , animStateType :: Int
  , animStateCurrFrame :: Int
  , animStateNextFrame :: Int
  }


data Model = Model
  { modelNumFrames :: Int
  , modelNumXyz :: Int
  , modelNumGLCmds :: Int

  , modelVerts :: [(Float,Float,Float)]
  , modelGLCmds :: [GLCmd]
  , modelLightNormIdxs :: [Int]

  , modelTexId :: GLuint
  , modelAnimState :: AnimState
  , modelScale :: Float
  }


animList :: [Anim]
animList =
  [ Anim   0  39  9 -- Stand
  , Anim  40  45 10 -- Run
  , Anim  46  53 10 -- Attack
  , Anim  54  57  7 -- Pain A
  , Anim  58  61  7 -- Pain B
  , Anim  62  65  7 -- Pain C
  , Anim  66  71  7 -- Jump
  , Anim  72  83  7 -- Flip
  , Anim  84  94  7 -- Salute
  , Anim  95 111 10 -- Fall Backward
  , Anim 112 122  7 -- Wave
  , Anim 123 134  6 -- Point
  , Anim 135 153 10 -- Crouch Stand
  , Anim 154 159  7 -- Crouch Walk
  , Anim 160 168 10 -- Crouch Attack
  , Anim 196 172  7 -- Crouch Pain
  , Anim 173 177  5 -- Crouch Death
  , Anim 178 183  7 -- Death Fall Backward
  , Anim 184 189  7 -- Death Fall Forward
  , Anim 190 197  7 -- Death Fall Backward Slowly
  , Anim 198 198  5 -- Boom
  ]

{-

loadModel
loadSkin
drawModel
drawFrame
setAnim
scaleModel
-
animate
processLighting
interpolate
renderFrame
--
anorms :: ['vertexnormals']
anormsDot ;: Float -- [shadedot_quant][26]
animlist  :: Anim

-}
