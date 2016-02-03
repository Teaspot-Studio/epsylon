module Game.Chunk(
    module Reexport
  , chunkActor
  ) where

import Linear 

import Game.Core 
import Game.Chunk.Data as Reexport

import Game.GoreAndAsh.Actor 

import Game.World.Data

chunkActor :: V2 Int -> AppActor ChunkId World Chunk
chunkActor v = makeActor $ \i -> pure (emptyChunk i v)