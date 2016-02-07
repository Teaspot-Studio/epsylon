module Game(
    mainWire
  , Game(..)
  ) where

import Control.Wire
import Linear 
import Linear.Affine
import Prelude hiding ((.), id)

import Consts
import Game.Core 
import Game.Data 

import Render.Texture

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL 

mainWire :: AppWire a (Maybe Game)
mainWire = (<|> pure Nothing) $ proc _ -> do 
  (_, _) <- dynamicSurface mainWindowName drawRectangle -< ()
  -- blitTexture mainWindowName Nothing Nothing -< t
  -- drawRectangle -< ()
  closed <- isWindowClosed mainWindowName -< ()
  returnA -< Just $ Game closed

drawRectangle :: AppWire (Surface, ()) ()
drawRectangle = liftGameMonad2 $ \s _ -> do
  surfaceFillRect s (Just $ Rectangle (P $ V2 100 100) (V2 100 100)) (V4 255 0 0 255)

-- | Outputs True if user hits close button
isWindowClosed :: WindowName -> AppWire a Bool
isWindowClosed wname = hold . mapE (const True) . windowClosed wname <|> pure False