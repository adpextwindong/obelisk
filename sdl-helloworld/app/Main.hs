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

type HV2 = V3 --TODO propagate this to indicate homoCoordsgenous coordinates
type Line = (HV2 CInt, HV2 CInt) -- After homoCoordsgenous coordinates its a v3


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

type M22Affine t = V3 (V3 t) -- TODO use this type alias??

idv3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) :: V3 (V3 CInt)

applyAffineTransform :: M22Affine CInt -> [Line] -> [Line]
applyAffineTransform t xs = fmap (bimap (t !* ) (t !*)) xs

appDTFloor :: M22Affine Double -> [Line] -> [Line]
appDTFloor t xs = fmap (bimap f f) xs
    where
        f = fmap floor . (t !*) . fmap fromIntegral :: V3 CInt -> V3 CInt
        --Convert to doubles, apply the transform then floor it

homoCoords :: (Num a) => V2 a -> HV2 a
homoCoords (V2 x y) = V3 x y 1

--TODO make sure the HV2 usage is correct
dropHomoCoords :: (Num a) => HV2 a -> V2 a
dropHomoCoords (V3 x y _) = V2 x y

--COLORS
white = SDL.V4 maxBound maxBound maxBound maxBound
red = SDL.V4 maxBound 0 0 maxBound
blue = SDL.V4 0 0 maxBound maxBound
black = SDL.V4 0 0 0 0

drawGridLine :: SDL.Renderer -> Line -> IO ()
drawGridLine screenRenderer (start, end) = line screenRenderer (dropHomoCoords start) (dropHomoCoords end) white

        --Center grid over origin, scale it by zoomFactor, rotate it by rotationFactor, move it to center of screen
gridT worldSize zoomFactor rotationFactor = translateToPDCenter !*! rotationT !*! zoom zoomFactor !*! centerToLocalOrigin
    where
        delta = fromIntegral worldSize / 2
        centerToLocalOrigin = translate (-delta) (-delta) :: M22Affine Double
        translateToPDCenter = translate (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)
        rotationT = rotation rotationFactor

drawGrid :: SDL.Renderer -> CInt -> Double -> Double -> IO ()
drawGrid screenRenderer worldSize zoomFactor rotationFactor = do
    let t = gridT worldSize zoomFactor rotationFactor
    let hlines = appDTFloor t (horizontal_lines worldSize) :: [Line]
    let vlines = appDTFloor t (vertical_lines worldSize) :: [Line]
    --let lines = applyAffineTransform (translate 320 240) vertical_lines
    forM_ hlines (drawGridLine screenRenderer)
    forM_ vlines (drawGridLine screenRenderer)
    where
        vertical_lines worldSize = [(homoCoords (V2 x 0), homoCoords (V2 x worldSize)) | x <- [0..worldSize]]
        horizontal_lines worldSize = [(homoCoords (V2 0 y), homoCoords (V2 worldSize y)) | y <- [0..worldSize]]

blastFmap4Tupple f (a,b,c,d) = (f a, f b, f c, f d)

--TODO hook this up to a 2D tilegrid array
drawGridTiles :: SDL.Renderer -> CInt -> Double -> Double -> IO ()
drawGridTiles screenRenderer worldSize zoomFactor rotationFactor = do
    let t = gridT worldSize zoomFactor rotationFactor
    let projectVertToPD = (dropHomoCoords . (fmap floor) . (t !*) . homoCoords . (fmap fromIntegral))
    let quads = [blastFmap4Tupple projectVertToPD (V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) | x <- [0..worldSize - 1], y <- [0..worldSize - 1]] :: [(Pos,Pos,Pos,Pos)]

    forM_ quads (\(vA,vB,vC,vD) -> do
        fillTriangle screenRenderer vA vB vC red
        fillTriangle screenRenderer vB vC vD blue
        )

(screenWidth, screenHeight) = (640, 480) :: (CInt,CInt)

worldSize = 10 :: CInt
zoomFactor = (fromIntegral screenHeight / fromIntegral worldSize) * 0.95 :: Double
--TODO mouse zoom handling

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    let title = "World Debug Window"

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
            let rotationFactor = elapsed_seconds --0.0

            drawGridTiles screenRenderer worldSize zoomFactor rotationFactor
            drawGrid screenRenderer worldSize zoomFactor rotationFactor

            SDL.updateWindowSurface window

            unless quitSignal loop


    loop

    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit
