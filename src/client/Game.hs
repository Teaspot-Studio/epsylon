module Game(
    mainWire
  , Game(..)
  ) where

import Game.Core 
import Game.Data 

mainWire :: AppWire a (Maybe Game)
mainWire = pure $ Just $ Game False