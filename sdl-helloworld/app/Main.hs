{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import qualified Data.Text as T
import qualified SDL.Vect as SDL
import qualified SDL
import qualified SDL.Event as SDL
import qualified SDL.Video.Renderer as SDL

import Data.Bifunctor
import Data.Word
import Linear
import SDL.Primitive as SDL

title = "My SDL Application"
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

type HV2 = V3 --TODO propagate this to indicate homogenous coordinates
type Line = (HV2 CInt, HV2 CInt) -- After homogenous coordinates its a v3

quads = [(V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) | x <- [0..20], y <- [0..20]]
--TODO make sdl_rect calls out of these vertexes for tile squares

vertical_lines = [(homo (V2 x 0), homo (V2 x 20)) | x <- [0..20]]
horizontal_lines = [(homo (V2 0 y), homo (V2 20 y)) | y <- [0..20]]
base_lines = vertical_lines ++ horizontal_lines

zoom :: (Num a) => a -> V3 (V3 a)
zoom scale = V3 (V3 scale 0     0)
                (V3 0     scale 0)
                (V3 0     0     1)

translate :: (Num a) => a -> a -> V3 (V3 a)
translate x y = V3 (V3 1 0 x)
                   (V3 0 1 y)
                   (V3 0 0 1)

rotation :: Double -> V3 (V3 Double)
rotation theta = V3 (V3 (cos theta) (-sin theta) 0)
                    (V3 (sin theta) (cos theta)  0)
                    (V3  0           0           1)

rotate_around :: Double -> (V2 Double) -> (V3 (V3 Double))
rotate_around theta (V2 x y) = (translate x y) !*! (rotation theta) !*! (translate (-x) (-y))

type M22Affine t = V3 t -- TODO use this type alias??

idv3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) :: V3 (V3 CInt)

applyAffineTransform :: V3 (V3 CInt) -> [Line] -> [Line]
applyAffineTransform t xs = fmap (bimap (t !* ) (t !*)) xs

appDTFloor :: V3 (V3 Double) -> [Line] -> [Line]
appDTFloor t xs = fmap (bimap f f) xs
    where
        f = fmap floor . (t !*) . fmap fromIntegral :: V3 CInt -> V3 CInt
        --Convert to doubles, apply the transform then floor it

homo :: (Num a) => V2 a -> V3 a
homo (V2 x y) = V3 x y 1

--TODO make sure the HV2 usage is correct
dropHomoCoords :: (Num a) => HV2 a -> V2 a
dropHomoCoords (V3 x y _) = V2 x y

--COLORS
white = SDL.V4 maxBound maxBound maxBound maxBound
black = SDL.V4 0 0 0 0

drawGridLine :: SDL.Renderer -> Line -> IO ()
drawGridLine screenRenderer (start, end) = line screenRenderer (dropHomoCoords start) (dropHomoCoords end) white

--TODO make this accept a grid size to accomodate world size
--TODO ROTATION HANDLING
drawGrid :: SDL.Renderer -> IO ()
drawGrid screenRenderer = do
    let t = translate 320 240 !*! zoom 20 !*! translate (-10) (-10)
    --let t = translate 320 240 !*! rotation (elapsed_seconds * pi / 4.0) !*! ...
    --Center grid over origin, scale it by 20, move it to center of screen
    let lines = appDTFloor t base_lines :: [Line]
    --let lines = applyAffineTransform (translate 320 240) vertical_lines
    forM_ lines (drawGridLine screenRenderer)


main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window

    screenSurface <- SDL.getWindowSurface window
    SDL.updateWindowSurface window

    screenRenderer <- SDL.createSoftwareRenderer screenSurface :: IO SDL.Renderer

    let loop = do
            SDL.clear screenRenderer
            SDL.surfaceFillRect screenSurface Nothing black
            events <- SDL.pollEvents :: IO [SDL.Event]
            let quitSignal = elem SDL.QuitEvent $ map SDL.eventPayload events
            --SDL.surfaceBlit garg Nothing screenSurface Nothing
            time <- SDL.ticks

            let elapsed_seconds = (fromIntegral (toInteger time)) / 1000.0

            drawGrid screenRenderer

            --forM_ horizontal_lines (\(start,end) -> line screenRenderer start end white)
            SDL.updateWindowSurface window

            unless quitSignal loop


    loop

    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit
