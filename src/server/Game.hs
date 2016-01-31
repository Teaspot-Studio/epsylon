module Game(
    mainWire
  , Game(..)
  , GameId(..)
  ) where

import Control.Wire 
import Data.Hashable 
import Prelude hiding (id, (.))

import Game.Core
import Game.Data
import Game.World 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync 

import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

mainWire :: AppActor GameId a Game 
mainWire = makeFixedActor (GameId 0) $ stateWire initalGame $ proc (_, g) -> do
  processWorlds -< g
  where
  initalGame = Game {
      gameWorlds = H.empty
    }

  processWorlds :: AppWire Game Game
  processWorlds = proc g -> do 
    addEvent <- mapE (fmap $ const worldActor) . never -< ()
    (delEvent :: Event (S.Seq WorldId)) <- never -< ()
    ws <- runActor'$ remoteActorCollectionServer S.empty -< (g, addEvent, delEvent)
    returnA -< g {
        gameWorlds = fromSeq $ fmap worldId ws `S.zip` ws
      }

-- | Make hashmap from sequence
fromSeq :: (Hashable i, Eq i) => S.Seq (i, a) -> H.HashMap i a
fromSeq = F.foldl' (\acc (i, a) -> H.insert i a acc) H.empty