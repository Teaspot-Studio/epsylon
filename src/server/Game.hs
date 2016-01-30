module Game(
    mainWire
  , Game(..)
  ) where

import GHC.Generics 
import Control.DeepSeq 

import Game.Core
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

mainWire :: AppActor GameId a Game 
mainWire = makeFixedActor (GameId 0) $ pure Game