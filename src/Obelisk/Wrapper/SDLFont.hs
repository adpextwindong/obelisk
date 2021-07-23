module Obelisk.Wrapper.SDLFont where

import Data.Text
import Data.Word
import Linear
import qualified SDL
import qualified SDL.Font

import Control.Monad.IO.Class (MonadIO(..))

class Monad m => SDLFont m where
    renderSolidText :: SDL.Font.Font -> V4 Word8 -> Text -> m SDL.Surface

renderSolidText' :: (MonadIO m) => SDL.Font.Font -> V4 Word8 -> Text -> m SDL.Surface
renderSolidText' = SDL.Font.solid