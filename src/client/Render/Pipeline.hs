module Render.Pipeline(
    mainPipeline
  , initPipeline
  , initStorage
  , getCurrentDpi
  ) where

import Control.Monad.IO.Class
import Data.Text

import qualified Graphics.UI.GLFW as GLFW

import Game.GoreAndAsh
import Game.GoreAndAsh.LambdaCube
import Game.GoreAndAsh.GLFW

import LambdaCube.GL as LambdaCubeGL

-- | Name of main graphic pipeline that could be used to uniquely refer to it
mainPipeline :: Text
mainPipeline = "mainPipeline"

-- | Initializes graphic pipeline and creates main window
initPipeline :: (MonadLambdaCube m, MonadGLFW m) => GameMonadT m ()
initPipeline = do
  win <- liftIO $ initWindow "Epsylon" 640 640
  setCurrentWindowM $ Just win
  lambdacubeAddPipeline ["./shaders"] "main.lc" mainPipeline $ do
    defObjectArray "objects" Triangles $ do
      "position"  @: Attribute_V2F
      "uv"        @: Attribute_V2F
    defObjectArray "strings" Triangles $
      "position"  @: Attribute_V2F
    defUniforms $ do
      "time"           @: Float
      "diffuseTexture" @: FTexture2D
      "windowWidth"    @: Int
      "windowHeight"   @: Int
      "minCorner"      @: V2F
      "maxCorner"      @: V2F
      "rotation"       @: Float

-- | Initialize GLFW window with given name and size and OpenGL context
initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ GLFW.WindowHint'ContextVersionMajor 3
      , GLFW.WindowHint'ContextVersionMinor 3
      , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      , GLFW.WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win

-- | Creates storage for main pipeline
initStorage :: MonadLambdaCube m => GameMonadT m GLStorage
initStorage = do
  (sid, storage) <- lambdacubeCreateStorage mainPipeline
  lambdacubeRenderStorageFirst sid
  return storage

-- | Get DPI of current monitor
getCurrentDpi :: (MonadGLFW m, MonadIO m) => GameMonadT m (Maybe Int)
getCurrentDpi = do
  mw <- getCurrentWindowM
  case mw of
    Nothing -> return Nothing
    Just w -> do
      mm <- liftIO $ GLFW.getWindowMonitor w
      case mm of
        Nothing -> return Nothing
        Just mon -> do
          mvm <- liftIO $ GLFW.getVideoMode mon
          case mvm of
            Nothing -> return Nothing
            Just vm -> do
              (mwidth, _) <- liftIO $ GLFW.getMonitorPhysicalSize mon
              let mdw = fromIntegral mwidth / (25.4 :: Double)
              return . Just . ceiling $ fromIntegral (GLFW.videoModeWidth vm) / mdw
