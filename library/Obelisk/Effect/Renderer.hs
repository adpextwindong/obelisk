{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Obelisk.Effect.Renderer where

import Control.Monad.Reader
import qualified SDL
import qualified SDL.Primitive as SDL
import Control.Lens
import Linear
import Foreign.C.Types

import Obelisk.Config
import Obelisk.State
import Obelisk.Math.Vector
import Obelisk.Math.Homogenous
import Obelisk.Wrapper.SDLRenderer
import Obelisk.Engine.DDA

--COLORS
white :: SDL.Color
white = SDL.V4 maxBound maxBound maxBound maxBound
red :: SDL.Color
red = SDL.V4 maxBound 0 0 maxBound
blue :: SDL.Color
blue = SDL.V4 0 0 maxBound maxBound
black :: SDL.Color
black = SDL.V4 0 0 0 0

--GodBolt Colors
backgroundColor :: SDL.Color
backgroundColor = SDL.V4 34 34 34 maxBound
gridColor :: SDL.Color
gridColor = SDL.V4 63 63 63 maxBound
filledTileColor :: SDL.Color
filledTileColor = SDL.V4 51 51 102 maxBound
doorTileColor :: SDL.Color
doorTileColor = SDL.V4 102 51 102 maxBound
arrowColor :: SDL.Color
arrowColor = SDL.V4 255 51 51 maxBound

wallTypeToColor :: WallType -> SDL.Color
wallTypeToColor FW = filledTileColor
wallTypeToColor EW = backgroundColor
wallTypeToColor DW = doorTileColor

class Monad m => Renderer m where
    clearScreen :: m ()
    drawScreen :: m ()
    fillBackground :: m ()
    drawDebug :: Vars -> m ()

type SDLCanDraw m = (SDLRenderer m, MonadReader Config m)

clearScreen' :: SDLCanDraw m => m ()
clearScreen' = do
    renderer <- asks cRenderer
    clearRenderer renderer

drawScreen' :: SDLCanDraw m => m ()
drawScreen' = do
    window <- asks cWindow 
    updateWindowSurface window

fillBackground' :: SDLCanDraw m => m ()
fillBackground' = do
    surface <- asks cSurface
    surfaceFillScreenRect surface backgroundColor

type GridTransform = M22Affine Double

drawDebug' :: SDLCanDraw m => Vars -> m ()
drawDebug' gs = do
    let ws = worldSize . world $ gs
    screenWidth <- asks cScreenWidth
    screenHeight <- asks cScreenHeight

    let zoomLimiter = 0.95
    let zoomFactor = fromIntegral screenHeight / fromIntegral ws * zoomLimiter

    --TODO rotation focus mechanism
    let rotationFactor = vectorAngle. direction $ player gs
    let focus = Just (position . player $ gs)

    let tPDCenter = translateToPDCenter screenWidth screenHeight
    let gtp = gridT ws zoomFactor rotationFactor focus tPDCenter

    drawGridTiles (world gs) gtp
    drawGrid ws gtp
    drawPlayer (player gs) gtp
    drawRaycastIntersectionSimple (player gs) gtp

---------------------------------------------------------------

drawGrid :: SDLCanDraw m => CInt -> GridTransform -> m ()
drawGrid ws t = do
    screenRenderer <- asks cRenderer

    let hlines = appDTFloor t (horizontal_lines ws) :: [Line]
    let vlines = appDTFloor t (vertical_lines ws) :: [Line]

    forM_ hlines drawGridLine
    forM_ vlines drawGridLine 
    where
        vertical_lines ws = [(homoCoords (V2 x 0), homoCoords (V2 x ws)) | x <- [0..ws]]
        horizontal_lines ws = [(homoCoords (V2 0 y), homoCoords (V2 ws y)) | y <- [0..ws]]

drawGridLine :: (SDLCanDraw m) => Line -> m ()
drawGridLine (start, end) = do
    screenRenderer <- asks cRenderer
    drawLine screenRenderer (dropHomoCoords start) (dropHomoCoords end) gridColor

drawGridTiles :: (SDLCanDraw m) => WorldTiles -> GridTransform -> m ()
drawGridTiles world t = do
    let ws = worldSize world
    screenRenderer <- asks cRenderer 

    let projectVertToPD = dropHomoCoords . fmap floor . (t !*) . homoCoords . fmap fromIntegral
    let inds = [(x,y) | x <- [0..ws -1], y <- [0..ws - 1]]
    let quads = [blastFmap4Tupple projectVertToPD (V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) | x <- [0..ws - 1], y <- [0..ws - 1]] :: [(SDL.Pos,SDL.Pos,SDL.Pos,SDL.Pos)]

    forM_ (zip inds quads) (\((x,y),(vA,vB,vC,vD)) -> do
        let tileColor = wallTypeToColor $ mapTiles world !! fromIntegral y !! fromIntegral x

        fillTriangle screenRenderer vA vB vC tileColor
        fillTriangle screenRenderer vB vC vD tileColor
        )

drawRaycastIntersectionSimple :: (SDLCanDraw m) => PVars -> GridTransform -> m ()
drawRaycastIntersectionSimple player t = do
    let pAngle = vectorAngle $ direction player
    let pOrigin = Step (position player ^._x) (position player ^._y)
    let intersections = take 10 $ rayPath pAngle pOrigin :: [(DDAStep, Double)]

    let pointToScreenSpace = dropHomoCoords . fmap floor . (t !*) . homoCoords
    let intersectionPosXs = fmap pointToScreenSpace $ intersectionPositions $ fmap fst intersections

    screenRenderer <- asks cRenderer
    forM_ intersectionPosXs (\pos -> do
            circle screenRenderer pos 3 (V4 255 255 0 maxBound)
        )

drawPlayer :: (SDLCanDraw m) => PVars -> GridTransform -> m ()
drawPlayer player gtp = do
    screenRenderer <- asks cRenderer

    let px = position player ^._x
    let py = position player ^._y

    let t = gtp !*! translate px py

    --Draw Circle
    let circle_pos = dropHomoCoords . fmap floor . (t !*) . homoCoords $ V2 0.0 0.0
    let circle_radius = 5
    circle screenRenderer circle_pos circle_radius white

    let arrowT = gtp !*! translate px py !*! rotation (vectorAngle . direction $ player)
    let apDT t =  dropHomoCoords . fmap floor . (t !*)
                                                --                 |
    --TODO figure out a better way to handle the scaling done here V
    let arrow_line = (homoCoords $ V2 0.0 0.0 , homoCoords $ V2 10.0 0.0)
    let (arrow_p0, arrow_p1) = blastFmapPair (apDT arrowT) arrow_line

    --Draw the line body of the arrow
    drawLine screenRenderer arrow_p0 arrow_p1 white

    --Draw Arrow
    let baseArrow = (homoCoords $ V2 0.0 (-0.2), homoCoords $ V2 0.7 0.0, homoCoords $ V2 0.0 0.2) :: (HV2 Double, HV2 Double, HV2 Double)
                                                                            --     |
                    --TODO figure out a better way to handle the scaling done here V
    let (arrowVA, arrowVB, arrowVC) = blastFmap3Tupple (apDT (arrowT !*! translate 0.5 0.0 !*! zoomT 0.5)) baseArrow

    fillTriangle screenRenderer arrowVA arrowVB arrowVC arrowColor


-------------------------------------------------------------------
translateToPDCenter :: CInt -> CInt -> M22Affine Double
translateToPDCenter screenWidth screenHeight = translate (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)

gridT :: CInt -> Double -> Double -> Maybe (V2 Double) -> M22Affine Double -> GridTransform
gridT worldSize zoomFactor rotationFactor Nothing translateToPDCenter = translateToPDCenter !*! rotationT !*! zoomT zoomFactor !*! centerToLocalOrigin
    where
        delta = fromIntegral worldSize / 2
        centerToLocalOrigin = translate (-delta) (-delta) :: M22Affine Double
        rotationT = rotation rotationFactor

gridT worldSize zoomFactor rotationFactor (Just focus) translateToPDCenter = translateToPDCenter !*! zt !*! rotationT !*! playerToLocalOrigin
    where
        zt = zoomT zoomFactor
        wsMid = fromIntegral worldSize / 2.0
        playerToLocalOrigin = translate (-focus ^._x) (-focus ^._y)  :: M22Affine Double
        rotationT = rotation ((-pi/2.0) - rotationFactor)

blastFmapPair :: (t -> b) -> (t, t) -> (b, b)
blastFmapPair f (a,b) = (f a, f b)
blastFmap3Tupple :: (t -> c) -> (t, t, t) -> (c, c, c)
blastFmap3Tupple f (a,b,c) = (f a, f b, f c)
blastFmap4Tupple :: (t -> d) -> (t, t, t, t) -> (d, d, d, d)
blastFmap4Tupple f (a,b,c,d) = (f a, f b, f c, f d)