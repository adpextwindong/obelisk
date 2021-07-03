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
white = SDL.V4 maxBound maxBound maxBound maxBound :: Color
red = SDL.V4 maxBound 0 0 maxBound :: Color
blue = SDL.V4 0 0 maxBound maxBound :: Color
black = SDL.V4 0 0 0 0 :: Color

--GodBolt Colors
backgroundColor= SDL.V4 34 34 34 maxBound :: Color
filledTileColor = SDL.V4 51 51 102 maxBound :: Color
doorTileColor = SDL.V4 102 51 102 maxBound :: Color
arrowColor = SDL.V4 255 51 51 maxBound :: Color
gridColor = SDL.V4 63 63 63 maxBound :: Color

drawGridLine :: SDL.Renderer -> Line -> IO ()
drawGridLine screenRenderer (start, end) = line screenRenderer (dropHomoCoords start) (dropHomoCoords end) gridColor

data GridTParams = GridTParams {
                        worldSize :: CInt,
                        zoomFactor :: Double,
                        rotationFactor :: Double
                   }

translateToPDCenter = translate (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)

        --Center grid over origin, scale it by zoomFactor, rotate it by rotationFactor, move it to center of screen
gridT :: GridTParams -> M22Affine Double
gridT (GridTParams worldSize zoomFactor rotationFactor) = translateToPDCenter !*! rotationT !*! zoom zoomFactor !*! centerToLocalOrigin
    where
        delta = fromIntegral worldSize / 2
        centerToLocalOrigin = translate (-delta) (-delta) :: M22Affine Double
        rotationT = rotation rotationFactor

drawGrid :: SDL.Renderer -> GridTParams -> IO ()
drawGrid screenRenderer gtp@(GridTParams worldSize _ _) = do
    let t = gridT gtp
    let hlines = appDTFloor t (horizontal_lines worldSize) :: [Line]
    let vlines = appDTFloor t (vertical_lines worldSize) :: [Line]
    --let lines = applyAffineTransform (translate 320 240) vertical_lines
    forM_ hlines (drawGridLine screenRenderer)
    forM_ vlines (drawGridLine screenRenderer)
    where
        vertical_lines worldSize = [(homoCoords (V2 x 0), homoCoords (V2 x worldSize)) | x <- [0..worldSize]]
        horizontal_lines worldSize = [(homoCoords (V2 0 y), homoCoords (V2 worldSize y)) | y <- [0..worldSize]]

blastFmap3Tupple f (a,b,c) = (f a, f b, f c)
blastFmap4Tupple f (a,b,c,d) = (f a, f b, f c, f d)

--TODO hook this up to a 2D tilegrid array
drawGridTiles :: SDL.Renderer -> GridTParams -> IO ()
drawGridTiles screenRenderer gtp@(GridTParams worldSize _ _) = do
    let t = gridT gtp
    let projectVertToPD = (dropHomoCoords . (fmap floor) . (t !*) . homoCoords . (fmap fromIntegral))
    let quads = [blastFmap4Tupple projectVertToPD (V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) | x <- [0..worldSize - 1], y <- [0..worldSize - 1]] :: [(Pos,Pos,Pos,Pos)]

    forM_ quads (\(vA,vB,vC,vD) -> do
        fillTriangle screenRenderer vA vB vC filledTileColor
        fillTriangle screenRenderer vB vC vD filledTileColor
        )

drawPlayerDirection :: SDL.Renderer -> (Double, Double) -> GridTParams -> IO ()
drawPlayerDirection screenRenderer (px, py) gtp = do
          --let t = translateToPDCenter !*! zoom 200.0 -- TODO change this
          let t = (gridT gtp) !*! translate px py !*! zoom 0.4



          let baseArrow = (homoCoords $ V2 0.0 (-0.2), homoCoords $ V2 0.7 0.0, homoCoords $ V2 0.0 0.2) :: (HV2 Double, HV2 Double, HV2 Double)
          let (arrowVA, arrowVB, arrowVC) = blastFmap3Tupple (dropHomoCoords . (fmap floor) . (t !*)) baseArrow

          fillTriangle screenRenderer arrowVA arrowVB arrowVC arrowColor

(screenWidth, screenHeight) = (640, 480) :: (CInt,CInt)

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

    --CONSTS
    let worldSize = 10 :: CInt
    let zoomFactor = (fromIntegral screenHeight / fromIntegral worldSize) * 0.95 :: Double
    let p = (5.5, 5.5)

    let loop = do
            SDL.clear screenRenderer
            SDL.surfaceFillRect screenSurface Nothing backgroundColor
            events <- SDL.pollEvents :: IO [SDL.Event]
            let quitSignal = elem SDL.QuitEvent $ map SDL.eventPayload events
            --SDL.surfaceBlit garg Nothing screenSurface Nothing
            time <- SDL.ticks

            let elapsed_seconds = (fromIntegral (toInteger time)) / 1000.0
            let rotationFactor = elapsed_seconds --0.0

            let gtp = GridTParams worldSize zoomFactor 0.0 --rotationFactor

            drawGridTiles screenRenderer gtp
            drawGrid screenRenderer gtp
            drawPlayerDirection screenRenderer p gtp

            SDL.updateWindowSurface window

            unless quitSignal loop


    loop

    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit
