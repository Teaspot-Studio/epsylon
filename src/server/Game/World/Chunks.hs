module Game.World.Chunks(
    ChunkArray
  , emptyChunkArray
  ) where

import Control.DeepSeq 
import GHC.Generics 

import Data.Array.Repa 
import Data.Word 
import qualified Data.Vector.Unboxed as V

-- | Two dimensional array of chunk ids
newtype ChunkArray = ChunkArray { _unChunkArray :: Array U DIM2 Word32 }
  deriving (Generic)

instance NFData ChunkArray where
  rnf (ChunkArray a) = a `deepSeqArray` ()

emptyChunkArray :: ChunkArray
emptyChunkArray = ChunkArray . fromUnboxed (ix2 0 0) $ V.empty