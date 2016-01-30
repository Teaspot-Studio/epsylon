module Game(
    mainWire
  , Game(..)
  ) where

import GHC.Generics 
import Control.DeepSeq 

import Game.Core 

data Game = Game {
    gameExit :: Bool
  }
  deriving (Generic)

instance NFData Game 

mainWire :: AppWire a (Maybe Game)
mainWire = pure $ Just $ Game False