module Game.Data(
    Game(..)
  ) where

import GHC.Generics 
import Control.DeepSeq 

data Game = Game {
    gameExit :: Bool
  }
  deriving (Generic)

instance NFData Game 
