module Render.Texture(
    dynamicSurface
  , blitTexture
  , withRenderer
  , withRendererM
  ) where

import Control.Wire 
import Prelude hiding (id, (.))
import Data.Text

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL as SDL

import Game.Core 

-- | Execute wire with renderer for given window.
-- 
-- Note: inhibits until Window isn't found.
--
-- TODO: move to SDL module.
withRenderer :: MonadSDL m 
  => Text -- ^ Name of window
  -> (Renderer -> GameWire m a b) -- ^ Action with renderer
  -> GameWire m a b
withRenderer wname rw = go 
  where
  go = mkGen $ \_ _ -> do 
    mwr <- sdlGetWindowM wname
    case mwr of 
      Nothing -> return (Left (), go)
      Just (_, r) -> return (Left (), rw r)

-- | Execute wire with renderer for given window.
-- 
-- Note: inhibits until Window isn't found.
--
-- TODO: move to SDL module.
withRendererM :: MonadSDL m 
  => Text -- ^ Name of window
  -> (Renderer -> GameMonadT m r) -- ^ Intialization of resource
  -> (r -> GameWire m a b) -- ^ Wire with resource
  -> GameWire m a b
withRendererM wname mr rw = go 
  where
  go = mkGen $ \_ _ -> do 
    mwr <- sdlGetWindowM wname
    case mwr of 
      Nothing -> return (Left (), go)
      Just (_, r) -> do 
        r' <- mr r 
        return (Left (), rw r')

-- | Execute wire with window object
-- 
-- Note: inhibits until Window isn't found.
--
-- TODO: move to SDL module.
withWindowM :: MonadSDL m 
  => Text -- ^ Name of window
  -> (Window -> GameMonadT m r) -- ^ Intialization of resource
  -> (r -> GameWire m a b) -- ^ Wire with resource
  -> GameWire m a b
withWindowM wname mr rw = go 
  where
  go = mkGen $ \_ _ -> do 
    mwr <- sdlGetWindowM wname
    case mwr of 
      Nothing -> return (Left (), go)
      Just (w, _) -> do 
        r' <- mr w
        return (Left (), rw r')

-- | Wraps rendering wire to render into texture
dynamicSurface :: 
     Text -- ^ Name of window
  -> AppWire (Surface, a) b -- ^ Rendering wire that output is directed to texture
  -> AppWire a (Surface, b) -- ^ Result is texture and rendering wire output
dynamicSurface wname drawWire = withWindowM wname initSurf renderSurf
  where
  initSurf w = (w,) <$> getWindowSurface w

  renderSurf (w, s) = proc a -> do  
    b <- drawWire -< (s, a)
    liftGameMonad $ updateWindowSurface w -< ()
    returnA -< (s, b)

-- | Blits input texture into window surface
blitTexture :: 
     Text -- ^ Name of window
  -> Maybe (Rectangle Int) -- ^ Source region, 'Nothing' is full region
  -> Maybe (Rectangle Int) -- ^ Destination region, 'Nothing' is full region
  -> AppWire Texture ()
blitTexture wname sr dr = withRenderer wname $ \r -> proc t -> do 
  liftGameMonad1 (\t -> SDL.copy r t (rconv sr) (rconv dr)) -< t 
  where 
    rconv = (fmap fromIntegral <$>)