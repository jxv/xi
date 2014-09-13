{-# LANGUAGE RecordWildCards #-}

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
  { xiClearColor = (0.2, 0.2, 0.2, 1.0)
  , xiClearBufferBits = gl_COLOR_BUFFER_BIT .|.
                              gl_DEPTH_BUFFER_BIT
  , xiSwapBuffers = swapBuffersCallback
  , xiModelViewMat = eye4
  , xiProjectionMat = eye4
  }

startFrame :: Xi -> IO ()
startFrame Xi{..} = do
  (uncurryN glClearColor) xiClearColor
  glClear xiClearBufferBits

endFrame :: Xi -> IO ()
endFrame Xi{..} = do
  glFlush
  xiSwapBuffers

