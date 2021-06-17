{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import qualified Data.Text as T
import SDL.Vect
import qualified SDL
import qualified SDL.Event as SDL

import SDL.Image
import Linear


title = "My SDL Application"
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

vertical_lines = [((V2 x 0), (V2 x 64)) | x <- [0..64]]   :: [(V2 CInt, V2 CInt)]
horizontal_lines = [((V2 0 y), (V2 64 y)) | y <- [0..64]] :: [(V2 CInt, V2 CInt)]


main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window

    garg <- SDL.Image.load "GARG_4.png"

    screenSurface <- SDL.getWindowSurface window
    let white = SDL.V4 maxBound maxBound maxBound maxBound
    SDL.surfaceFillRect screenSurface Nothing white
    SDL.updateWindowSurface window

    let loop = do
            events <- SDL.pollEvents :: IO [SDL.Event]
            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        --Do stuff here, we can pass a monad in or somen
            SDL.surfaceBlit garg Nothing screenSurface Nothing
            SDL.updateWindowSurface window

            unless quit loop


    loop

    SDL.destroyWindow window
    SDL.quit
