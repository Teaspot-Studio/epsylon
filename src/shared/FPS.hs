module FPS(
    makeFPSBounder
  , waitFPSBound
  ) where

import Control.Concurrent
import Control.Monad 

type FPSBound = MVar ()

-- Temporaly disabled, as Windows doesn't have GHC.Events 

-- | Creates mvar that fills periodically with given fps
makeFPSBounder :: Int -> IO FPSBound
makeFPSBounder fps = newEmptyMVar

-- | Wait until next FPS value is reached when the function unblocks
waitFPSBound :: FPSBound -> IO ()
waitFPSBound _ = return ()