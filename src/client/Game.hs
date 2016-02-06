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

import Game.GoreAndAsh.SDL 

mainWire :: AppWire a (Maybe Game)
mainWire = (<|> pure Nothing) $ proc _ -> do 
  (t, _) <- dynamicTexture mainWindowName RGBA8888 (V2 600 800) drawRectangle -< ()
  blitTexture mainWindowName Nothing Nothing -< t
  returnA -< Just $ Game False

drawRectangle :: AppWire a ()
drawRectangle = proc _ -> returnA -< ()