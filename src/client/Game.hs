{-# LANGUAGE Arrows #-}
module Game(
    mainWire
  , Game(..)
  ) where

import Control.Wire
import Prelude hiding ((.), id)

import Data.Int
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.IO.Class
import Linear

import Game.Core
import Game.Data

import Render.Sprite
import Render.Pipeline

import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW
import Game.GoreAndAsh.LambdaCube

import LambdaCube.GL as LambdaCubeGL hiding (V2)

mainWire :: AppWire a (Maybe Game)
mainWire = withInit (const initStorage) renderWire

-- | Wire that renders the scene
renderWire :: GLStorage -> AppWire a (Maybe Game)
renderWire storage = (<|> pure Nothing) $ proc _ -> do
  w <- nothingInhibit . liftGameMonad getCurrentWindowM -< ()
  closed <- isWindowClosed -< ()
  (aspect, width, height) <- updateWinSize -< w
  t <- timeF -< ()
  globalUniforms -< (aspect, t, width, height)
  renderScene storage -< ()
  glfwFinishFrame -< w
  returnA -< Just $ Game closed
  where
  -- Updates storage uniforms
  globalUniforms :: AppWire (Float, Float, Int32, Int32) ()
  globalUniforms = liftGameMonad1 $ \(aspect, t, w, h) -> liftIO $
    LambdaCubeGL.updateUniforms storage $ do
      --"viewMat" @= return (cameraMatrix t)
      --"projMat" @= return (projMatrix aspect)
      "windowWidth" @= return w
      "windowHeight" @= return h

-- | Updates LambdaCube window size
updateWinSize :: AppWire GLFW.Window (Float, Int32, Int32)
updateWinSize = liftGameMonad1 $ \win -> do
  (w, h) <- liftIO $ GLFW.getWindowSize win
  lambdacubeUpdateSize (fromIntegral w) (fromIntegral h)
  return (fromIntegral w / fromIntegral h, fromIntegral w, fromIntegral h)

-- | Swaps frame
glfwFinishFrame :: AppWire GLFW.Window ()
glfwFinishFrame = liftGameMonad1 $ liftIO . GLFW.swapBuffers

-- | Outputs True if user hits close button
isWindowClosed :: AppWire a Bool
isWindowClosed = hold . mapE (const True) . windowClosing <|> pure False

-- | Draws objects to screen
renderScene :: GLStorage -> AppWire a ()
renderScene storage = proc _ -> do
  renderUISprite storage "./media/texture.png" -< (V2 (-1) (-1.0), V2 1.0 1.0, 0)
  returnA -< ()
