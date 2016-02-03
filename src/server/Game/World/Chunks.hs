module Game.World.Chunks(
    ChunkArray
  , emptyChunkArray
  , simulateChunkArray
  ) where

import Control.Wire 
import Prelude hiding (id, (.))

import Control.DeepSeq 
import GHC.Generics 

import Data.Array.Repa 
import qualified Data.Vector.Unboxed as V

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H 

import Game.Core
import Game.Chunk

-- | Two dimensional array of chunks
data ChunkArray = ChunkArray { 
  -- | Stores two dimensional array of chunk ids
    chunksIdArray :: Array U DIM2 Int
  -- | Stores simulated chunks
  , chunksLoaded :: HashMap ChunkId Chunk
  }
  deriving (Generic)

instance NFData ChunkArray where
  rnf (ChunkArray {..}) = chunksIdArray `deepSeqArray` chunksLoaded `deepseq` ()

-- | Make initial chunk array
emptyChunkArray :: ChunkArray
emptyChunkArray = ChunkArray {
    chunksIdArray = fromUnboxed (ix2 0 0) $ V.empty
  , chunksLoaded = H.empty 
  }

-- | Controller for chunks, determines when chunk should awake or stoped.
simulateChunkArray :: AppWire a ChunkArray 
simulateChunkArray = proc _ -> returnA -< emptyChunkArray