module Tga where


import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString as BS


data Tga = Tga
  { tgaHeader :: BS.ByteString
  , tgaWidth :: Word32
  , tgaHeight :: Word32
  , tgaBuffer :: BS.ByteString
  }


instance Binary Tga where
  put tga = do
    put (tgaHeader tga)
    let width = tgaWidth tga
    let height = tgaHeight tga
    let w = shiftL width 8
    let h = shiftL height 8
    let x = width - (shiftR w 8)
    let y = height - (shiftR h 8)
    putWord8 (fromIntegral x)
    putWord8 (fromIntegral w)
    putWord8 (fromIntegral y)
    putWord8 (fromIntegral h)
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
      , tgaBuffer = buffer
      }


