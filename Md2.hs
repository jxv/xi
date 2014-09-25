{-# LANGUAGE RecordWildCards  #-} 
{-# LANGUAGE DeriveGeneric  #-} 

module Md2 where


import Control.Applicative
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Word
import GHC.Generics (Generic)
import qualified Data.ByteString as BS


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
  , triTexidx :: (Word16, Word16, Word16)
  } deriving (Generic, Show)


type TexCoord = (Word16, Word16)


data GLCmdVert = GLCmdVert
  { glCmdVertS :: Float
  , glCmdVertT :: Float
  , glCmdVertIdx :: Word32
  } deriving (Generic, Show)


data Md2 = Md2
  { md2Header :: Md2Header
  } deriving (Show)


instance Binary Md2Header
instance Binary TriVert
instance Binary Tri
instance Binary GLCmdVert


