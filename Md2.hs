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
import Data.Word
import Data.Int
import GHC.Generics (Generic)
import Foreign.Storable (Storable(..), sizeOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL


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
  } deriving (Generic, Show)


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
  } deriving (Generic, Show)


type TexCoord = (Word16, Word16)


data GLCmdVert = GLCmdVert
  { glCmdVertS :: Float
  , glCmdVertT :: Float
  , glCmdVertIdx :: Word32
  } deriving (Generic, Show)


data Md2 = Md2
  { md2Header :: Md2Header
  , md2SkinBuf :: BL.ByteString
  , md2Frames :: [Frame]
  } deriving (Show)


instance Binary TriVert
instance Binary Tri
instance Binary GLCmdVert


instance Binary Md2Header where
  put Md2Header{..} = do
    return ()
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


instance Binary Md2 where
  put Md2{..} = do
    return ()
  get = do
    header@Md2Header{..} <- get
    buffer <- getLazyByteString (fromIntegral md2HeaderOfsEnd - 68)
    let getBuf n o s
          = BL.take (fromIntegral n * fromIntegral s)
          . BL.drop (fromIntegral o)
          $ buffer
    let skinBuf = getBuf md2HeaderNumSkins (md2HeaderOfsSkins - 68) (size32 * 16)
    let frameBuf = getBuf md2HeaderNumFrames (md2HeaderOfsFrames - 68) md2HeaderFramesize

    let frames = runGet (sequence $ replicate (fromIntegral md2HeaderNumFrames) (getFrame $ fromIntegral md2HeaderNumXyz))
                        frameBuf
    return $ Md2
      { md2Header = header
      , md2SkinBuf = skinBuf
      , md2Frames = frames
      }
   where
    size32 = sizeOf (undefined :: Word32)


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


