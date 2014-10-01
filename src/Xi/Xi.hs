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
  }

bufferBit :: Buffer -> GLenum
bufferBit = \case
  ColorBuffer -> gl_COLOR_BUFFER_BIT
  DepthBuffer -> gl_DEPTH_BUFFER_BIT
  StencilBuffer -> gl_STENCIL_BUFFER_BIT

startFrame :: Xi -> IO ()
startFrame xi = do
  (uncurryN glClearColor) (xi ^. xiBackgroundColor)
  glClear $ foldr (.|.) 0 (map bufferBit (xi ^. xiActiveBuffers))

endFrame :: Xi -> IO ()
endFrame xi = do
  glFlush
  xi ^. xiSwapBuffers

