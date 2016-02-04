module Data.Range(
    Range(..)
  , rangeSingle
  ) where

import Control.DeepSeq
import Control.Monad (mzero)
import Data.Yaml 
import GHC.Generics (Generic)

-- | Range from min to max value
data Range a = Range {
  rangeMin :: !a
, rangeMax :: !a  
} deriving (Generic, Eq)

instance Show a => Show (Range a) where
  show Range{..} = "[" ++ show rangeMin ++ " .. " ++ show rangeMax ++ "]"

instance NFData a => NFData (Range a)

-- | Construct range from single value
rangeSingle :: a -> Range a 
rangeSingle a = Range a a

instance ToJSON a => ToJSON (Range a) where
  toJSON Range{..} = object [
      "min" .= rangeMin 
    , "max" .= rangeMax
    ]

instance FromJSON a => FromJSON (Range a) where
  parseJSON (Object v) = Range 
    <$> v .: "min"
    <*> v .: "max"
  parseJSON _ = mzero