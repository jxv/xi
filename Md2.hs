{-# LANGUAGE RecordWildCards  #-} 
{-# LANGUAGE DeriveGeneric  #-} 

module Md2 where


import Control.Applicative
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Word
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
  , md2HeaderNumS :: Word16
  , md2HeaderNumT :: Word16
  , md2HeaderNumTris :: Word32
  , md2HeaderNumGlcmds :: Word32
  , md2HeaderNumFrames :: Word32
  , md2HeaderOfsSkins :: Word32
  , md2HeaderOfsS :: Word16
  , md2HeaderOfsT :: Word16
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

-- Skin

data Md2 = Md2
  { md2Header :: Md2Header
  , md2SkinBuf :: BL.ByteString
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
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
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
          . BL.drop (fromIntegral o * fromIntegral s)
          $ buffer
    let skinBuf = getBuf md2HeaderNumSkins (md2HeaderOfsSkins - 68) (size32 * 16)
    let frameBuf = getBuf md2HeaderNumFrames (md2HeaderOfsFrames - 68) md2HeaderFramesize
    return $ Md2
      { md2Header = header
      , md2SkinBuf = skinBuf
      }
   where
    size32 = sizeOf (undefined :: Word32)


putFrame :: Frame -> Put
putFrame Frame{..} = do
  put frameScale
  put frameTranslate
  put frameName
  put frameTriVerts


getFrame :: Get Frame
getFrame = do
  scale <- get
  translate <- get
  name <- getByteString 16
  return $ Frame
    { frameScale = scale
    , frameTranslate = translate
    , frameName = name
    , frameTriVerts = []
    }


