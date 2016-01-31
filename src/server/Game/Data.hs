module Game.Data(
    Game(..)
  , GameId(..)
  ) where

import Control.DeepSeq 
import Data.HashMap.Strict (HashMap)
import GHC.Generics 

import Game.GoreAndAsh.Actor 

import Game.World.Data 

data Game = Game {
    gameWorlds :: HashMap WorldId World
  } deriving (Generic)

instance NFData Game 

newtype GameId = GameId { unGameId :: Int }
  deriving (Generic)

instance NFData GameId

data GameMessage 

instance ActorMessage GameId where
  type ActorMessageType GameId = GameMessage
  toCounter = unGameId
  fromCounter = GameId