{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.World.Data(
    World(..)
  , WorldId(..)
  ) where

import Control.DeepSeq
import Data.Text 
import GHC.Generics

import Game.World.Chunks
import Game.World.Shared

import Game.GoreAndAsh.Sync

data World = World {
  worldId :: !WorldId 
, worldName :: !Text
, worldChunks :: !ChunkArray
} deriving (Generic)

instance NFData World

instance RemoteActor WorldId World where
  type RemoteActorState WorldId = World 
  type RemoteActorId World = WorldId
