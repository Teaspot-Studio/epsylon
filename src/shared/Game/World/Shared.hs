module Game.World.Shared(
    WorldId(..)
  ) where

import Control.DeepSeq
import Data.Hashable 
import Data.Serialize
import GHC.Generics

import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync

newtype WorldId = WorldId { unWorldId :: Int } 
  deriving (Generic, Show, Eq)

instance NFData WorldId 
instance Hashable WorldId 
instance Serialize WorldId

data WorldMessage 

instance ActorMessage WorldId where
  type ActorMessageType WorldId = WorldMessage
  toCounter = unWorldId
  fromCounter = WorldId

data WorldNetMessage

instance NetworkMessage WorldId where 
  type NetworkMessageType WorldId = WorldNetMessage