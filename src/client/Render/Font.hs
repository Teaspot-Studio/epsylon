module Render.Font(
    renderUIString
  , PointSize(..)
  ) where

import Control.Wire
import Prelude hiding (id, (.))

import Control.Monad.IO.Class
import Data.Maybe
import Graphics.Text.TrueType as TrueType
import Linear

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import Game.Core
import Math
import Render.Pipeline

import Game.GoreAndAsh

import LambdaCube.GL as LambdaCubeGL hiding (V2)
import qualified LambdaCube.GL as LC
import LambdaCube.GL.Mesh as LambdaCubeGL

-- | Render given string with TrueType font (loaded via FontyFruity) at window coordinates [-1, 1]
--
-- Input: top left corner and bottom right corner, and rotation
renderUIString :: GLStorage -- ^ Lambda cube object storage aka scene
  -> FilePath -- ^ Path to font file
  -> PointSize -- ^ Size of font in points
  -> String -- ^ String to render
  -> AppWire (V2 Float, V2 Float, Float) ()
renderUIString storage fontPath fontSize s = withInit makeString renderString
  where
    makeString _ = do
      dpi <- fromMaybe 60 <$> getCurrentDpi
      liftIO $ do
        -- | Load font file
        Right font <- TrueType.loadFontFile fontPath
        let countours = countourMesh . concat $ getStringCurveAtPoint dpi (0, 0) [(font, fontSize, s)]

        -- load mesh
        gpuMesh <- LambdaCubeGL.uploadMeshToGPU countours
        LambdaCubeGL.addMeshToObjectArray storage "strings"
          ["minCorner", "maxCorner", "rotation"] gpuMesh

    renderString :: Object -> AppWire (V2 Float, V2 Float, Float) ()
    renderString obj = liftGameMonad1 $ \(minCorner, maxCorner, rot) ->  liftIO $ do
      let setter = LambdaCubeGL.objectUniformSetter obj
      uniformV2F "minCorner" setter . toLC $ minCorner
      uniformV2F "maxCorner" setter . toLC $ maxCorner
      uniformFloat "rotation" setter rot

-- | Mesh that covers all screen
countourMesh :: [VU.Vector (Float, Float)] -> LambdaCubeGL.Mesh
countourMesh vs = Mesh
  { mAttributes   = Map.fromList
      [ ("position",  A_V2F $ uncurry LC.V2 <$> VU.convert linesVec)
      ]
  , mPrimitive    = P_Triangles
  }
  where
    addFakePoint acc (v1, v2) = acc `VU.snoc` v1 `VU.snoc` v2 `VU.snoc` v1
    addFakePoints v = VU.foldl addFakePoint VU.empty (v `VU.zip` VU.drop 1 v)
    linesVec = VU.concat $ fmap addFakePoints vs
