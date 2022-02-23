{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
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

import Obelisk.Math.Hierarchy (M_World2PhysicalDevice, M_PhysicalDevice2World)
import Data.Tagged
import Data.Coerce

import Obelisk.Wrapper.SDLRenderer
import Obelisk.Wrapper.SDLInput
import Obelisk.Wrapper.SDLFont
import Obelisk.Engine.Ray (visitedPositions)

import Obelisk.Graphics.Primitives
import qualified Obelisk.Graphics.DebugUI as DUI
import Obelisk.Math.Homogenous (m22AffineIdD, dropHomoCoords, homoCoords)

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
    drawGraphicDebugWithMatrix :: Graphic (Shape Float) -> M22Affine Float -> m ()

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

drawGraphicDebugWithMatrix' :: (Debug m, SDLCanDraw m, SDLFont m, SDLInput m) => Graphic (Shape Float) -> M22Affine Float -> m ()
drawGraphicDebugWithMatrix' g gtp = do
    drawGraphic $ evalGraphic $ AffineT gtp g


drawDebug' :: (Debug m , SDLCanDraw m, SDLFont m, SDLInput m) => Vars -> m ()
drawDebug' = undefined
{- TODO UNDO AFTER WE FINISH MOUSELOOKRAYCASTGRAPHIC
drawDebug' gs = do
    let ws = worldSize . world $ gs
    screenWidth <- asks cScreenWidth
    screenHeight <- asks cScreenHeight
    --RAYCASTING CORE
    --let rays = genRays 320 (player gs) (fromIntegral ws)
    let visitedSet = S.unions $ visitedPositions gs <$> rays
    --

    --DEBUG UI, everything is in world coordinates
    let new_grid = DUI.worldGridGraphic ws
    let new_gridTiles = DUI.worldGridTilesGraphic (world gs) visitedSet
    let new_player = DUI.playerGraphic (player gs)
    let new_midline_intersections = DUI.midlineRaycastIntersectionsGraphic (player gs) ws

    let new_ui = GroupPrim "Debug UI" [new_gridTiles, new_grid, new_player,
            new_midline_intersections]

    --TODO use Data.Tagged on gtp to make sure it isn't misused.
    -- Grid To Player as center Local to Screen Affine Transformation
    let gtp = centerScreenOnWorldGrid ws screenWidth screenHeight

    drawGraphic $ evalGraphic $ AffineT gtp new_ui

    -- old_drawRaycastIntersectionSimple (player gs) gtp
    -- old_drawRaycastIntersections (player gs) gtp
    -- _ <- drawMouseLoc gtp
    --TODO draw sideRaycastIntersections
    return ()
-}
---------------------------------------------------------------
-- rayCount = 320 --TODO FIXME REVERT

-- type GridTransform = M22Affine Float

--TODO try cleaning this stuff with the Tagged phantom types
-- drawMouseLoc :: (SDLCanDraw m, SDLInput m, SDLFont m) => GridTransform -> m (Maybe (SDL.Rectangle CInt))
-- drawMouseLoc gtp = do
--     mloc <- getMouseAbsoluteLoc
--     targetSurface <- asks cSurface
--     font <- asks cFont

--     let textColor = V4 255 255 255 255

--     let worldLoc = pdToWorldPos gtp (fmap fromIntegral mloc)
--     let gridLoc = pdToGridPos gtp (fmap fromIntegral mloc)
--     let text = T.pack (show worldLoc ++ "  $$  " ++ show gridLoc)
--     --TODO this should be given an inv33 of gtp, and builds a graphic to append to the UI.

--     textSurface <- renderSolidText font textColor text

--     let position = Just (SDL.P (V2 50 50))

--     surfaceBlit textSurface Nothing targetSurface position

-- --Supposed to be applied to the gtp to get a PDtoWorld Matrix
pdToWorldT :: M_World2PhysicalDevice Float -> M_PhysicalDevice2World Float
pdToWorldT t = coerce @(M22Affine Float) (inv33 @_ (coerce t))

pdToWorldPos :: M_World2PhysicalDevice Float -> SDL.Point V2 Float -> V2 Float
pdToWorldPos t' (SDL.P pos) = dropHomoCoords . (unTagged (pdToWorldT t') !*) . homoCoords $ pos

rawPDtoWorldPos :: M22Affine Float -> SDL.Point V2 Float -> V2 Float
rawPDtoWorldPos t (SDL.P pos) = dropHomoCoords . (inv33 t !*) . homoCoords $ pos

-- pdToGridPos :: M22Affine Float -> SDL.Point V2 Float -> V2 CInt
-- pdToGridPos t (SDL.P pos) = dropHomoCoords . fmap floor . (pdToWorldT t !*) . homoCoords $ pos


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

--New version using the Data.Tagged phantom typed version
centerScreenOnWorldGrid :: CInt -> CInt -> CInt -> M_World2PhysicalDevice Float
centerScreenOnWorldGrid = coerce . rawCenterScreenOnWorldGrid
