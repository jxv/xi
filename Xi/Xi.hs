{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Xi.Xi
  ( makeXi
  , startFrame
  , endFrame
  ) where

------------------------------------------------------------------------------------------

import Xi.Imports
import Xi.Types

------------------------------------------------------------------------------------------

makeXi :: IO () -> Xi
makeXi swapBuffersCallback = Xi
  { _xiBackgroundColor = (0.2, 0.2, 0.2, 1.0)
  , _xiActiveBuffers = [ColorBuffer, DepthBuffer]
  , _xiSwapBuffers = swapBuffersCallback
  , _xiModelViewMat = eye4
  , _xiProjectionMat = eye4
  }

bufferBit :: Buffer -> GLenum
bufferBit = \case
  ColorBuffer -> gl_COLOR_BUFFER_BIT
  DepthBuffer -> gl_DEPTH_BUFFER_BIT
  StencilBuffer -> gl_STENCIL_BUFFER_BIT

startFrame :: Xi -> IO ()
startFrame Xi{..} = do
  (uncurryN glClearColor) _xiBackgroundColor
  glClear $ foldr (.|.) 0 (map bufferBit _xiActiveBuffers)

endFrame :: Xi -> IO ()
endFrame Xi{..} = do
  glFlush
  _xiSwapBuffers

