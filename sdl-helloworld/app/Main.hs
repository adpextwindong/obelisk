module Main where

import Control.Concurrent (threadDelay)
import Foreign.C.Types
--import Lib
import qualified Data.Text as T
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow (T.pack "My SDL Application") SDL.defaultWindow
    SDL.showWindow window

    screenSurface <- SDL.getWindowSurface window
    let white = SDL.V4 maxBound maxBound maxBound maxBound
    SDL.surfaceFillRect screenSurface Nothing white
    SDL.updateWindowSurface window

    threadDelay 2000000

    SDL.destroyWindow window
    SDL.quit