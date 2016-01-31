module Game(
    mainWire
  , Game(..)
  , GameId(..)
  ) where

import Game.Core
import Game.Data
import Game.GoreAndAsh.Actor 

mainWire :: AppActor GameId a Game 
mainWire = makeFixedActor (GameId 0) $ pure Game