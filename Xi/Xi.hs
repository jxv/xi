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
  { _backgroundColor = (0.2, 0.2, 0.2, 1.0)
  , _activeBuffers = [ColorBuffer, DepthBuffer]
  , _swapBuffers = swapBuffersCallback
  }

bufferBit :: Buffer -> GLenum
bufferBit = \case
  ColorBuffer -> gl_COLOR_BUFFER_BIT
  DepthBuffer -> gl_DEPTH_BUFFER_BIT
  StencilBuffer -> gl_STENCIL_BUFFER_BIT

startFrame :: Xi -> IO ()
startFrame xi = do
  (uncurryN glClearColor) (xi^.backgroundColor)
  glClear $ foldr (.|.) 0 (map bufferBit (xi^.activeBuffers))

endFrame :: Xi -> IO ()
endFrame xi = do
  glFlush
  xi^.swapBuffers

