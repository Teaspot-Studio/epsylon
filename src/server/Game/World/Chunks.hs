module Game.World.Chunks(
    ChunkArray
  , emptyChunkArray
  , simulateChunkArray
  ) where

import Control.Wire 
import Prelude hiding (id, (.))

import Game.Core

import Game.World.Chunks.Data
import Game.World.Data

-- | Controller for chunks, determines when chunk should awake or stoped.
simulateChunkArray :: AppWire World ChunkArray 
simulateChunkArray = proc _ -> returnA -< emptyChunkArray