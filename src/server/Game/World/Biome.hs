module Game.World.Biome(
    Biome(..)
  ) where

import Control.DeepSeq 
import Control.Monad (mzero)
import Data.Range 
import Data.Text 
import Data.Yaml
import GHC.Generics (Generic)

-- | Describing type of climatic class
data Biome = Biome {
  -- | Display name of biome
  biomeName :: !Text
  -- | Annual precipitation (mm) from minimal to maximum (62.5 ... 16000)
, biomePrecip :: !(Range Double)
  -- | Potential evapotranspiration ratio (0.125 ... 32)
, biomeEvapor :: !(Range Double)
} deriving (Generic, Show)

instance NFData Biome 

instance ToJSON Biome where 
  toJSON (Biome {..}) = object [
      "name" .= biomeName
    , "precip" .= biomePrecip
    , "evapor" .= biomeEvapor
    ]

instance FromJSON Biome where 
  parseJSON (Object v) = Biome 
    <$> v .: "name"
    <*> v .: "precip"
    <*> v .: "evapor"
  parseJSON _ = mzero