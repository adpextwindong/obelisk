module Obelisk.Config where

import qualified SDL
import qualified SDL.Font
import Foreign.C.Types ( CInt )

--Contains all the SDL context stuff
data Config = Config {
                cWindow :: SDL.Window ,
                cRenderer :: SDL.Renderer,
                cSurface :: SDL.Surface,
                cScreenWidth :: CInt,
                cScreenHeight :: CInt,
                cFont :: SDL.Font.Font
              }