module Obelisk.Config where

import qualified SDL

--Contains all the SDL context stuff
data Config = Config {
                cWindow :: SDL.Window ,
                cRenderer :: SDL.Renderer,
                cSurface :: SDL.Surface
                    
              }