{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Math(
    ToLC(..)
  , FromLC(..)
  ) where

import Linear
import qualified LambdaCube.GL as LC

-- | Converting to LambdaCube representation
class ToLC a b | a -> b where
  toLC :: a -> b

-- | Converting from LambdaCube representation
class FromLC a b | a -> b where
  fromLC :: a -> b

instance ToLC (V2 Float) (LC.V2 Float) where
  toLC (V2 a b) = LC.V2 a b

instance FromLC (LC.V2 Float) (V2 Float) where
  fromLC (LC.V2 a b) = V2 a b
