{-# LANGUAGE CPP #-}

module Xi.Debug
  ( debug
  ) where

------------------------------------------------------------------------------------------

import Xi.Imports

------------------------------------------------------------------------------------------

debug :: String -> IO ()
debug message =
#ifdef DEBUG
  hPutStrLn stderr message
#else
  return ()
#endif


