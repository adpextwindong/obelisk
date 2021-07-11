module Obelisk.Wrapper.SDLRenderer where

import qualified SDL
import qualified SDL.Primitive as SDL

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types

class Monad m => SDLRenderer m where
    updateWindowSurface :: SDL.Window -> m ()
    surfaceFillScreenRect :: SDL.Surface -> SDL.Color -> m ()
    clearRenderer :: SDL.Renderer -> m ()
    drawLine :: SDL.Renderer -> SDL.Pos -> SDL.Pos -> SDL.Color -> m ()
    fillTriangle :: SDL.Renderer -> SDL.Pos -> SDL.Pos -> SDL.Pos -> SDL.Color -> m ()
    circle :: SDL.Renderer -> SDL.Pos -> SDL.Radius -> SDL.Color -> m ()
    fillCircle :: SDL.Renderer -> SDL.Pos -> SDL.Radius -> SDL.Color -> m ()

updateWindowSurface' :: MonadIO m => SDL.Window -> m ()
updateWindowSurface' window = liftIO $ SDL.updateWindowSurface window

surfaceFillScreenRect' :: MonadIO m => SDL.Surface -> SDL.Color -> m ()
surfaceFillScreenRect' screenSurface = SDL.surfaceFillRect screenSurface Nothing

clearRenderer' :: MonadIO m => SDL.Renderer -> m ()
clearRenderer' = SDL.clear

drawLine' :: MonadIO m => SDL.Renderer -> SDL.Pos -> SDL.Pos -> SDL.Color -> m ()
drawLine' = SDL.line

fillTriangle' :: MonadIO m => SDL.Renderer -> SDL.Pos -> SDL.Pos -> SDL.Pos -> SDL.Color -> m ()
fillTriangle' = SDL.fillTriangle

circle' :: MonadIO m => SDL.Renderer -> SDL.Pos -> SDL.Radius -> SDL.Color -> m ()
circle' = SDL.circle 

fillCircle' :: MonadIO m => SDL.Renderer -> SDL.Pos -> SDL.Radius -> SDL.Color -> m ()
fillCircle' = SDL.fillCircle 
