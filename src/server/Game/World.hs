module Game.World(
    World(..)
  , WorldId(..)
  , worldActor
  ) where

import Control.Wire 
import Prelude hiding (id, (.))

import Game.Core
import Game.World.Data

import Game.Data 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 

worldActor :: AppActor WorldId Game World 
worldActor = makeActor $ \i -> stateWire (initalWorld i) $ mainController i
  where
  initalWorld i = World {
      worldId = i 
    , worldName = "Default world"
    } 

  mainController _ = proc (_, w) -> do 
    returnA -< w