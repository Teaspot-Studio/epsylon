module Game.World(
    World(..)
  , WorldId(..)
  , worldActor
  ) where

import Control.Wire 
import Prelude hiding (id, (.))

import Game.Core
import Game.Data 
import Game.World.Chunks
import Game.World.Data

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 

worldActor :: AppActor WorldId Game World 
worldActor = makeActor $ \i -> stateWire (initalWorld i) $ mainController i
  where
  initalWorld i = World {
      worldId = i 
    , worldName = "Default world"
    , worldChunks = emptyChunkArray
    } 

  mainController _ = proc (_, w) -> do 
    returnA -< w