module Game(
    mainWire
  , Game(..)
  ) where

import Game.Core 
import Game.Data 

import Render.Texture

mainWire :: AppWire a (Maybe Game)
mainWire = pure $ Just $ Game False