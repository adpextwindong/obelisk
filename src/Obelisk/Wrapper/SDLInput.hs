module Obelisk.Wrapper.SDLInput where

import Data.Word
import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types

class Monad m => SDLInput m where
    pollEventPayloads :: m [SDL.EventPayload]
    checkQuitSignal :: m Bool
    getTime :: m Word32
    getMouseAbsoluteLoc :: m (SDL.Point SDL.V2 CInt)

pollEventPayloads' :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads' = liftIO $ map SDL.eventPayload <$> SDL.pollEvents

checkQuitSignal' :: MonadIO m => m Bool
checkQuitSignal' = liftIO . fmap (elem SDL.QuitEvent) $ map SDL.eventPayload <$> SDL.pollEvents

getTime' :: MonadIO m => m Word32
getTime' = SDL.ticks

getMouseAbsoluteLoc' :: MonadIO m => m (SDL.Point SDL.V2 CInt)
getMouseAbsoluteLoc' = SDL.getAbsoluteMouseLocation