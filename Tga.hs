module Tga where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Word
import Data.ByteString


data Tga = Tga
  { tgaHeader :: ByteString
  , tgaWidth  :: Word32
  , tgaHeight :: Word32
  , tgaSize   :: Word8
  , tgaBuffer :: ByteString
  } deriving (Show, Eq)


instance Binary Tga where
  put tga = do
    put (tgaHeader tga)
    let w = shiftL (tgaWidth tga) 8
    let h = shiftL (tgaHeight tga)  8
    let x = (tgaWidth tga) - (shiftR w 8)
    let y = (tgaHeight tga) - (shiftR h 8)
    putWord8 (fromIntegral x)
    putWord8 (fromIntegral w)
    putWord8 (fromIntegral y)
    putWord8 (fromIntegral h)
    putWord8 (tgaSize tga)
    putWord8 0x00
    put (tgaBuffer tga)
  get = do
    header <- getByteString 12
    x <- getWord8
    w <- getWord8
    y <- getWord8
    h <- getWord8
    s <- getWord8
    _ <- getWord8
    let width = fromIntegral w * 256 + fromIntegral x
    let height = fromIntegral h * 256 + fromIntegral y
    let imgSize = (fromIntegral s `div` 8) * fromIntegral width * fromIntegral height
    buffer <- getByteString imgSize
    return $ Tga
      { tgaHeader = header
      , tgaWidth = width
      , tgaHeight = height
      , tgaSize = s
      , tgaBuffer = buffer
      }


