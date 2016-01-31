module Game.Data(
    Game(..)
  , GameId(..)
  ) where

import GHC.Generics 
import Control.DeepSeq 

import Game.GoreAndAsh.Actor 

data Game = Game 
  deriving (Generic)

instance NFData Game 

newtype GameId = GameId { unGameId :: Int }
  deriving (Generic)

instance NFData GameId

data GameMessage 

instance ActorMessage GameId where
  type ActorMessageType GameId = GameMessage
  toCounter = unGameId
  fromCounter = GameId
