{-# LANGUAGE CPP #-}

module Xi.Debug
  ( debug
  ) where

debug :: String -> IO ()
debug message =
#ifdef DEBUG
  hPutStrLn stderr message
#else
  return ()
#endif


