module Render.Texture(
    dynamicTexture
  ) where

import Control.Wire 
import Prelude hiding (id, (.))
import Data.Text 
import Linear 

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL 

import Game.Core 

-- | Wraps rendering wire to render into texture
dynamicTexture :: 
     Text -- ^ Name of window
  -> PixelFormat -- ^ Format for texture
  -> V2 Int -- ^ Size for texture
  -> AppWire a b -- ^ Rendering wire that output is directed to texture
  -> AppWire a (Texture, b) -- ^ Result is texture and rendering wire output
dynamicTexture wname pf sz drawWire = initTex
  where
  initTex = mkGen $ \_ _ -> do 
    mwr <- sdlGetWindowM wname
    case mwr of 
      Nothing -> return (Left (), initTex)
      Just (_, r) -> do
        t <- createTexture r pf TextureAccessTarget (fromIntegral <$> sz) 
        return (Left (), renderTex t)

  renderTex t = proc a -> do  
    liftGameMonad $ glBindTexture t -< ()
    b <- drawWire -< a
    liftGameMonad $ glUnbindTexture t  -< ()
    returnA -< (t, b)