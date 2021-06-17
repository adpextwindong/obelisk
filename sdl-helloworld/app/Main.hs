{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import qualified Data.Text as T
import SDL.Vect
import qualified SDL
import qualified SDL.Event as SDL

import Data.Bifunctor
import Data.Word
import SDL.Image
import Linear
import SDL.Primitive

title = "My SDL Application"
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

type Line = (V3 CInt, V3 CInt)
vertical_lines = [(homo (V2 x 0), homo (V2 x 64)) | x <- [0..10]]
horizontal_lines = [(homo (V2 0 y), homo (V2 64 y)) | y <- [0..10]]
base_lines = vertical_lines ++ horizontal_lines

zoom_matrix :: (Num a) => a -> V3 (V3 a)
zoom_matrix scale = V3 (V3 scale 0     0)
                       (V3 0     scale 0)
                       (V3 0     0     1)

translate :: (Num a) => a -> a -> V3 (V3 a)
translate x y = V3 (V3 1 0 x)
                   (V3 0 1 y)
                   (V3 0 0 1)

rotation_matrix :: Double -> V3 (V3 Double)
rotation_matrix theta = V3 (V3 (cos theta) (-sin theta) 0)
                           (V3 (sin theta) (cos theta)  0)
                           (V3 0 0 1)

rotate_around :: Double -> (V2 Double) -> (V3 (V3 Double))
rotate_around theta (V2 x y) = (translate_matrix x y) !*! (rotation_matrix theta) !*! (translate_matrix (-x) (-y))

idv3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) :: V3 (V3 CInt)
apply_transform :: V3 (V3 CInt) -> [Line] -> [Line]
apply_transform t xs = fmap (bimap (*! t) (*! t)) xs

homo :: (Num a) => V2 a -> V3 a
homo (V2 x y) = V3 x y 1

dropHomo :: (Num a) => V3 a -> V2 a
dropHomo (V3 x y _) = V2 x y

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window

    garg <- SDL.Image.load "GARG_4.png"

    screenSurface <- SDL.getWindowSurface window
    let white = SDL.V4 maxBound maxBound maxBound maxBound
    let black = SDL.V4 0 0 0 0
    SDL.surfaceFillRect screenSurface Nothing black
    SDL.updateWindowSurface window

    screenRenderer <- SDL.createSoftwareRenderer screenSurface

    let loop = do
            events <- SDL.pollEvents :: IO [SDL.Event]
            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        --Do stuff here, we can pass a monad in or somen
          --SDL.surfaceBlit garg Nothing screenSurface Nothing

            let lines = apply_transform (idv3) vertical_lines
            forM_ (apply_transform (zoom_matrix 10) vertical_lines) (\(start, end) -> line screenRenderer (dropHomo start) (dropHomo end) white)

            --forM_ horizontal_lines (\(start,end) -> line screenRenderer start end white)
            SDL.updateWindowSurface window

            unless quit loop


    loop

    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit
