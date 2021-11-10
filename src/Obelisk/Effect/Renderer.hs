{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Obelisk.Effect.Renderer where

import Control.Monad.Reader
import qualified SDL
import qualified SDL.Primitive as SDL
import qualified SDL.Font
import Control.Lens
import Linear
import Foreign.C.Types
import Data.Bool
import qualified Data.Set as S
import qualified Data.Text as T

import Obelisk.Config
import Obelisk.State
import Obelisk.Types.Wall
import Obelisk.Effect.Debug
import Obelisk.Math.Vector
import Obelisk.Math.Homogenous
import Obelisk.Wrapper.SDLRenderer
import Obelisk.Wrapper.SDLInput
import Obelisk.Wrapper.SDLFont
import Obelisk.Engine.Ray (visitedPositions, genRays)

import Obelisk.Graphics.Primitives
import qualified Obelisk.Graphics.DebugUI as DUI

--TODO this stuff is too low level for the renderer, we should move this shit out of here
--once we're done with the debug UI we can have a similar layer for the actual game
--COLORS
white :: SDL.Color
white = SDL.V4 maxBound maxBound maxBound maxBound
red :: SDL.Color
red = SDL.V4 maxBound 0 0 maxBound
blue :: SDL.Color
blue = SDL.V4 0 0 maxBound maxBound
black :: SDL.Color
black = SDL.V4 0 0 0 0
yellow :: SDL.Color
yellow = SDL.V4 255 255 0 maxBound

--GodBolt Colors
backgroundColor :: SDL.Color
backgroundColor = SDL.V4 34 34 34 maxBound
filledTileColor :: SDL.Color
filledTileColor = SDL.V4 51 51 102 maxBound
doorTileColor :: SDL.Color
doorTileColor = SDL.V4 102 51 102 maxBound

wallTypeToColor :: WallType -> SDL.Color
wallTypeToColor FW = filledTileColor
wallTypeToColor EW = backgroundColor
wallTypeToColor DW = doorTileColor

class Monad m => Renderer m where
    clearScreen :: m ()
    drawScreen :: m ()
    fillBackground :: m ()
    drawDebug :: Vars -> m ()
    drawGraphicDebug :: Graphic (Shape Float) -> m ()

--TODO consider SDLFont being in this
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

drawGraphicDebug' :: (Debug m, SDLCanDraw m, SDLFont m, SDLInput m) => Graphic (Shape Float) -> m ()
drawGraphicDebug' g = do
    -- Grid To Player as center Local to Screen Affine Transformation
    let gtp = centerScreenOnWorldGrid 10 640 480
--dprint $ evalGraphic $ AffineT gtp g
    drawGraphic $ evalGraphic $ AffineT gtp g

drawDebug' :: (Debug m , SDLCanDraw m, SDLFont m, SDLInput m) => Vars -> m ()
drawDebug' gs = do
    let ws = worldSize . world $ gs
    screenWidth <- asks cScreenWidth
    screenHeight <- asks cScreenHeight
    -- Grid To Player as center Local to Screen Affine Transformation
    let gtp = centerScreenOnWorldGrid ws screenWidth screenHeight

    -- let visitedSet = S.unions $ visitedPositions gs <$> genRays rayCount (player gs)
    let rays = genRays 320 (player gs) (fromIntegral ws)
    let visitedSet = S.unions $ visitedPositions gs <$> rays
    --TODO benchmark genRays
    --TODO redo the old debugui for all the rays we cast.

    -- old_drawGridTiles (world gs) visitedSet gtp --TODO REVERT
    -- dprint visitedSet

    -- let tempVisitedSet = S.fromList tzzz  --TODO REVERT
    -- old_drawGridTiles (world gs) tempVisitedSet gtp

    let new_grid = DUI.worldGridGraphic ws
    let new_gridTiles = DUI.worldGridTilesGraphic (world gs) visitedSet
    let new_player = DUI.playerGraphic (player gs)
    let new_midline_intersections = DUI.midlineRaycastIntersectionsGraphic (player gs) ws

    let new_ui = AffineT gtp $ GroupPrim "Debug UI" [new_gridTiles, new_grid, new_player,
            new_midline_intersections]

    -- dprint $ vectorAngle $ direction (player gs)
    drawGraphic $ evalGraphic new_ui

    -- old_drawRaycastIntersectionSimple (player gs) gtp
    -- old_drawRaycastIntersections (player gs) gtp
    -- _ <- drawMouseLoc gtp
    --TODO draw sideRaycastIntersections
    return ()

---------------------------------------------------------------
-- rayCount = 320 --TODO FIXME REVERT

type GridTransform = M22Affine Float

drawMouseLoc :: (SDLCanDraw m, SDLInput m, SDLFont m) => GridTransform -> m (Maybe (SDL.Rectangle CInt))
drawMouseLoc t = do
    mloc <- getMouseAbsoluteLoc
    targetSurface <- asks cSurface
    font <- asks cFont

    let textColor = V4 255 255 255 255

    let worldLoc = pdToWorldPos t (fmap fromIntegral mloc)
    let gridLoc = pdToGridPos t (fmap fromIntegral mloc)
    let text = T.pack (show worldLoc ++ "  $$  " ++ show gridLoc)

    textSurface <- renderSolidText font textColor text

    let position = Just (SDL.P (V2 50 50))

    surfaceBlit textSurface Nothing targetSurface position

--------------------------------------------------------------------------------

-- | Maps eval'd primitives to their SDLCanDraw draw calls
drawGraphic :: SDLCanDraw m => Graphic (Shape CInt) -> m ()
drawGraphic (EvaldGP _ evald_xs) = mapM_ drawGraphic evald_xs
drawGraphic (EvaldP (Line start end color))           = (\sr -> drawLine sr start end color) =<< asks cRenderer
drawGraphic (EvaldP (Circle center radius color))     = (\sr -> circle sr center radius color) =<< asks cRenderer
drawGraphic (EvaldP (FillTriangle v0 v1 v2 color))    = (\sr -> fillTriangle sr v0 v1 v2 color) =<< asks cRenderer
drawGraphic (EvaldP (FillCircle center radius color)) = (\sr -> fillCircle sr center radius color) =<< asks cRenderer
drawGraphic (AffineT _ _) = undefined --TODO figure out a way for this to be statically known that EvaldP contains no AffineT's
--------------------------------------------------------------------------------

--TODO FINISH PORTING THE REST TO THE NEW GRAPHIC API
--------------------------------------------------------------------------------

translateToPDCenter :: CInt -> CInt -> M22Affine Float
translateToPDCenter screenWidth screenHeight = translate (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)

gridT :: CInt -> Float -> Float -> Maybe (V2 Float) -> M22Affine Float -> GridTransform
gridT worldSize zoomFactor rotationFactor Nothing translateToPDCenter = translateToPDCenter !*! rotationT !*! zoomT zoomFactor !*! centerToLocalOrigin
    where
        delta = fromIntegral worldSize / 2
        centerToLocalOrigin = translate (-delta) (-delta) :: M22Affine Float
        rotationT = rotation rotationFactor

gridT worldSize zoomFactor rotationFactor (Just focus) translateToPDCenter = translateToPDCenter !*! zt !*! rotationT !*! playerToLocalOrigin
    where
        zt = zoomT zoomFactor
        wsMid = fromIntegral worldSize / 2.0
        playerToLocalOrigin = translate (-focus ^._x) (-focus ^._y)  :: M22Affine Float
        rotationT = rotation ((-pi/2.0) - rotationFactor)

blastFmapPair :: (t -> b) -> (t, t) -> (b, b)
blastFmapPair f (a,b) = (f a, f b)
blastFmap3Tupple :: (t -> c) -> (t, t, t) -> (c, c, c)
blastFmap3Tupple f (a,b,c) = (f a, f b, f c)
blastFmap4Tupple :: (t -> d) -> (t, t, t, t) -> (d, d, d, d)
blastFmap4Tupple f (a,b,c,d) = (f a, f b, f c, f d)

pointToScreenSpace :: GridTransform -> V2 Float -> V2 CInt
pointToScreenSpace t = dropHomoCoords . fmap floor . (t !*) . homoCoords

pdToWorldT :: M22Affine Float -> M22Affine Float
pdToWorldT = inv33

pdToWorldPos t (SDL.P pos) = dropHomoCoords . (pdToWorldT t !*) . homoCoords $ pos
pdToGridPos t (SDL.P pos) = dropHomoCoords . fmap floor . (pdToWorldT t !*) . homoCoords $ pos

-- Grid To Player as center Local to Screen Affine Transformation
--The gtp
centerScreenOnWorldGrid :: CInt -> CInt -> CInt -> GridTransform
centerScreenOnWorldGrid ws screenWidth screenHeight = gridT ws zoomFactor rotationFactor focus tPDCenter
    where
    --TODO rotation focus mechanism
    -- let rotationFactor = bool 0.0 (vectorAngle. direction $ player gs) $ rotateToPView gs
    -- let focus = bool Nothing (Just (position . player $ gs)) $ rotateToPView gs
        zoomLimiter = 0.95
        zoomFactor = fromIntegral screenHeight / fromIntegral ws * zoomLimiter
        rotationFactor = 0.0
        focus = Nothing
        tPDCenter = translateToPDCenter screenWidth screenHeight
