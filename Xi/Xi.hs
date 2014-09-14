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
  { xiBackgroundColor = (0.2, 0.2, 0.2, 1.0)
  , xiActiveBuffers = [ColorBuffer, DepthBuffer]
  , xiSwapBuffers = swapBuffersCallback
  , xiModelViewMat = eye4
  , xiProjectionMat = eye4
  }

bufferBit :: Buffer -> GLenum
bufferBit = \case
  ColorBuffer -> gl_COLOR_BUFFER_BIT
  DepthBuffer -> gl_DEPTH_BUFFER_BIT
  StencilBuffer -> gl_STENCIL_BUFFER_BIT

startFrame :: Xi -> IO ()
startFrame Xi{..} = do
  (uncurryN glClearColor) xiBackgroundColor
  glClear $ foldr (.|.) 0 (map bufferBit xiActiveBuffers)

endFrame :: Xi -> IO ()
endFrame Xi{..} = do
  glFlush
  xiSwapBuffers

