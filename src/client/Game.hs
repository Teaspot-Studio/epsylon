module Game(
    mainWire
  , Game(..)
  ) where

import Control.Wire 
import Prelude hiding ((.), id)
import Linear 

import Consts
import Game.Core 
import Game.Data 

import Render.Texture

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL 

mainWire :: AppWire a (Maybe Game)
mainWire = (<|> pure Nothing) $ proc _ -> do 
  (t, _) <- dynamicTexture mainWindowName RGBA8888 (V2 600 800) drawRectangle -< ()
  blitTexture mainWindowName Nothing Nothing -< t
  closed <- isWindowClosed mainWindowName -< ()
  returnA -< Just $ Game closed

drawRectangle :: AppWire a ()
drawRectangle = proc _ -> returnA -< ()

-- | Outputs True if user hits close button
isWindowClosed :: WindowName -> AppWire a Bool
isWindowClosed wname = hold . mapE (const True) . windowClosed wname <|> pure False