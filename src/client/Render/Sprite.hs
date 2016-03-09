module Render.Sprite(
    renderUISprite
  ) where

import Control.Wire
import Prelude hiding (id, (.))

import Codec.Picture as Juicy
import Control.Monad.IO.Class
import Linear

import qualified Data.Map as Map
import qualified Data.Vector as V

import Game.Core
import Math

import Game.GoreAndAsh

import LambdaCube.GL as LambdaCubeGL hiding (V2)
import qualified LambdaCube.GL as LC
import LambdaCube.GL.Mesh as LambdaCubeGL

-- | Render given image (loaded via JuicyPixels) at window coordinates [-1, 1]
--
-- Input: top left corner and bottom right corner, and rotation
renderUISprite :: GLStorage -- ^ Lambda cube object storage aka scene
  -> FilePath -- ^ Path to texture
  -> AppWire (V2 Float, V2 Float, Float) ()
renderUISprite storage filename = withInit makeSprite renderSprite
  where
    makeSprite _ = liftIO $ do
      -- load mesh
      gpuMesh <- LambdaCubeGL.uploadMeshToGPU quad
      obj <- LambdaCubeGL.addMeshToObjectArray storage "objects"
        ["minCorner", "maxCorner", "rotation", "diffuseTexture"] gpuMesh
      -- load image and upload texture
      Right img <- Juicy.readImage filename
      texData <- LambdaCubeGL.uploadTexture2DToGPU img

      return (obj, texData)

    renderSprite :: (Object, TextureData) -> AppWire (V2 Float, V2 Float, Float) ()
    renderSprite (obj, texData) = liftGameMonad1 $ \(minCorner, maxCorner, rot) ->  liftIO $ do
      let setter = LambdaCubeGL.objectUniformSetter obj
      uniformV2F "minCorner" setter . toLC $ minCorner
      uniformV2F "maxCorner" setter . toLC $ maxCorner
      uniformFloat "rotation" setter rot
      uniformFTexture2D "diffuseTexture" setter texData

-- | Mesh that covers all screen
quad :: LambdaCubeGL.Mesh
quad = Mesh
  { mAttributes   = Map.fromList
      [ ("position",  A_V2F $ V.fromList [ LC.V2 1 1, LC.V2 1 (-1), LC.V2 (-1) (-1)
                                         , LC.V2 1 1, LC.V2 (-1) (-1), LC.V2 (-1) 1])
      , ("uv",        A_V2F $ V.fromList [ LC.V2 1 1, LC.V2 1 0, LC.V2 0 0
                                         , LC.V2 1 1, LC.V2 0 0, LC.V2 0 1])
      ]
  , mPrimitive    = P_Triangles
  }
