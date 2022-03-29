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

import Control.Monad.State
import Obelisk.Config
import Obelisk.State
import Obelisk.Types.Wall
import Obelisk.Effect.Debug
import Obelisk.Math.Vector
import Obelisk.Math.Homogenous

import Obelisk.Math.Hierarchy (M_World2PhysicalDevice, M_PhysicalDevice2World)
import Data.Tagged
import Data.Coerce
import Data.Maybe

import Obelisk.Wrapper.SDLRenderer
import Obelisk.Wrapper.SDLInput
import Obelisk.Wrapper.SDLFont

import Obelisk.Graphics.DebugUI
import Obelisk.Graphics.Primitives
import qualified Obelisk.Graphics.DebugUI as DUI
import Obelisk.Math.Homogenous (m22AffineIdD, dropHomoCoords, homoCoords)
import Obelisk.Graphics.ColorConstants
import Obelisk.Engine.Ray

class Monad m => Renderer m where
    clearScreen :: m ()
    drawScreen :: m ()
    fillBackground :: m ()
    drawDebug :: Vars -> m ()
    drawGraphicDebug :: Graphic Float -> m ()
    drawGraphicDebugWithMatrix :: Graphic Float -> M22Affine Float -> m ()
    blitSurfaceToWindowSurface :: SDL.Surface -> m ()

--TODO consider SDLFont being in this
type SDLCanDraw m = (SDLRenderer m, MonadReader Config m)

clearScreen' :: SDLCanDraw m => m ()
clearScreen' = do
    renderer <- asks cRenderer
    clearRenderer renderer

blitSurfaceToWindowSurface' :: (SDLRenderer m, MonadReader Config m, MonadIO m) => SDL.Surface -> m ()
blitSurfaceToWindowSurface' s = do
  windowSurface <- SDL.getWindowSurface =<< asks cWindow
  surfaceBlit s Nothing windowSurface Nothing
  return ()


drawScreen' :: SDLCanDraw m => m ()
drawScreen' = do
    window <- asks cWindow
    updateWindowSurface window

fillBackground' :: SDLCanDraw m => m ()
fillBackground' = do
    surface <- asks cSurface
    surfaceFillScreenRect surface backgroundColor

--TODO removem monad IO due to SDL.copy
drawGraphicDebug' :: (MonadIO m, Debug m, SDLCanDraw m, SDLFont m, SDLInput m) => Graphic Float -> m ()
drawGraphicDebug' g = do
    -- Grid To Player as center Local to Screen Affine Transformation
    let gtp = centerScreenOnWorldGrid 10 640 480
--dprint $ evalGraphic $ AffineT gtp g
    drawGraphic . evalGraphic $ AffineT gtp g

--TODO removem monad IO due to SDL.copy
drawGraphicDebugWithMatrix' :: (MonadIO m, Debug m, SDLCanDraw m, SDLFont m, SDLInput m) => Graphic Float -> M22Affine Float -> m ()
drawGraphicDebugWithMatrix' g gtp = do
    drawGraphic . evalGraphic $ AffineT gtp g


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
drawGraphic :: (Debug m, MonadIO m, SDLCanDraw m) => Graphic CInt -> m ()
drawGraphic (EvaldGP _ evald_xs) = mapM_ drawGraphic evald_xs
drawGraphic (EvaldP (Line start end color))           = (\sr -> drawLine sr start end color) =<< asks cRenderer
drawGraphic (EvaldP (Circle center radius color))     = (\sr -> circle sr center radius color) =<< asks cRenderer
drawGraphic (EvaldP (FillTriangle v0 v1 v2 color))    = (\sr -> fillTriangle sr v0 v1 v2 color) =<< asks cRenderer
drawGraphic (EvaldP (FillRectangle v0 v1 color))      = (\sr -> fillRectangle sr v0 v1 color) =<< asks cRenderer
drawGraphic (EvaldP (FillCircle center radius color)) = (\sr -> fillCircle sr center radius color) =<< asks cRenderer
drawGraphic (EvaldP cr@(CopyRect texture txtsize s@(V2 srcX srcY) dstart dend)) = do
  let srcRect = SDL.Rectangle (SDL.P s) txtsize
  --TODO calculate which slice of the texture to copy based on part of the wall
  --dprint $ show dstart ++ " " ++ show dend
  let dstRect = SDL.Rectangle (SDL.P dstart) (dend - dstart)
  renderer <- asks cRenderer
  --dprint cr
  --TODO texture selection
  SDL.copy renderer texture Nothing (Just dstRect)

drawGraphic (AffineT _ _) = undefined --TODO figure out a way for this to be statically known that EvaldP contains no AffineT's
--------------------------------------------------------------------------------

--TODO FINISH PORTING THE REST TO THE NEW GRAPHIC API
--------------------------------------------------------------------------------

--New version using the Data.Tagged phantom typed version
centerScreenOnWorldGrid :: CInt -> CInt -> CInt -> M_World2PhysicalDevice Float
centerScreenOnWorldGrid = coerce . rawCenterScreenOnWorldGrid

--TODO expose visibility set
raycast :: (SDLCanDraw m, MonadState Vars m) => V2 Float -> m (Graphic Float)
raycast lookingAtWorldPos = do
  let rayCount = 320 -- TODO float out
  let screenWidth = 640
  let screenHeight = 480

  pp <- player <$> get
  p <- position . player <$> get
  w <- world <$> get

  let ws = worldSize w

  let ray = normalize $ lookingAtWorldPos - p
  let mousePlayer = pp {
    direction = ray,
    camera_plane = normalize $ ray *! rotation2 (pi / 2.0)
  }

  let fst3 (a,b,c) = a

  let rayAnglePairs = rayHeads rayCount mousePlayer
  let rays = fmap fst rayAnglePairs
  let angles = fmap snd rayAnglePairs

  let paths = fst3 . shootRay (fromIntegral ws) p <$> rays
  let (wallPoints, visitedV) = stScreenWalkRaysForWall w p paths

  -- Screen Graphic
  projType <- projectionType . config <$> get
  textures <- asks cTextures

  let wallWidth = fromIntegral $ screenWidth `div` fromIntegral rayCount
  let wallHeight = 64 --Wall Height 64, Player Height 32?
  let screenMiddle = fromIntegral screenHeight / 2
  let wallFromMaybe (mInt,index, rayAngle) = case mInt of
                                    Nothing -> Nothing
                                    Just (intpos, intindex) -> let distanceToSlice = case projType of
                                                                    FishEye -> norm $ intpos - p
                                                                    Permadi -> rayAngle * distance p intpos

                                                                   --TODO distance to the projection plane?
                                                                   --TODO check if the screen is inverted
                                                                   projectedWallHeight = wallHeight / distanceToSlice
                                                                   wallTop = screenMiddle - projectedWallHeight
                                                                   wallBottom = screenMiddle + projectedWallHeight
                                                                   wallLeft = index * wallWidth
                                                                   wallRight = (index + 1) * wallWidth in

                                                               --TODO tweak CopyRect to contains SDL.Rectangles instead
                                                               --TODO bitmap offset from intersection
                                                               Just $ Prim $ CopyRect (fromJust textures) (V2 0 0) (V2 64 64) (V2 wallLeft wallTop) (V2 wallRight wallBottom)
                                                               --Just $ Prim $ FillRectangle (V2 wallLeft wallTop) (V2 wallRight wallBottom) filledTileColor

  let walls = catMaybes $ wallFromMaybe <$> zip3 wallPoints [0..] angles

  return $ GroupPrim "PlayerPOV" walls


--
-- DEMOS
--

--raycast at mouse look demo : grenderMouseLook mouseLookRaycastGraphicM
mouseLookRaycastGraphicM :: (SDLCanDraw m, Debug m, MonadState Vars m) => V2 Float -> m (Graphic Float)
mouseLookRaycastGraphicM  lookingAtWorldPos = do
    screen <- raycast lookingAtWorldPos


    p <- position . player <$> get
    w <- world <$> get
    let ws = worldSize w
    camZoom <- camZoomScale <$> get

    let ray = normalize $ lookingAtWorldPos - p

    pp <- player <$> get
    let mousePlayer = pp {
      direction = ray,
      camera_plane = normalize $ ray *! rotation2 (pi / 2.0)
    }

    let circleAt color c = Prim $ Circle c (floor camZoom) color --TODO Scale on camzoom
    let fst3 (a,b,c) = a
    --TODO scale this to 64x64 and benchmark
    let tempRayCount = 320 :: Int
    let rayAnglePairs = rayHeads tempRayCount mousePlayer :: [(V2 Float, Float)]
    let rays = fmap fst rayAnglePairs
    let rayAngles = fmap snd rayAnglePairs

    let paths = fst3 . shootRay (fromIntegral ws) p <$> rays :: [[Intersection]]
    let (wallPoints, visitedV) = stScreenWalkRaysForWall w p paths

    let rayCastPoints = fmap (circleAt yellow . fst) <$> wallPoints

    let rayCastPointsG = GroupPrim "16 ScreenWidth Raycast Points" . catMaybes $ rayCastPoints

    --Single ray path and intersections
    let (_singlePath, vints, hints) = shootRay (fromIntegral ws) p ray

    -- Overhead field of view done with triangles of adjacent intersections and the player as tri verts
    let triangleAt a b c = Prim $ FillTriangle a b c yellow
    let rayIntersections = fmap fst . catMaybes $ wallPoints

    --Field of View
    let fieldOfViewTestTris = uncurry (triangleAt p) <$> zip rayIntersections (tail rayIntersections)

    let playerCircle = Prim $ Circle p (floor camZoom) white


    screenMode <- viewMode <$> get
    -- screen <- oldScreenGraphic wallPoints rayAngles 640 480 tempRayCount


    return $ case screenMode of
      --WorldSpace
      OverheadDebug -> GroupPrim "MouseLookScreenRayIntersections" $ [
            worldGridTilesGraphic w visitedV,
            worldGridGraphic ws,
            playerCircle,
            GroupPrim "Vertical Intersections" $ (red `circleAt`) <$> vints,
            GroupPrim "Horizontal Intersections" $ (blue `circleAt`) <$> hints,
            rayCastPointsG] -- ++ fieldOfViewTestTris
      --ScreenSpace
      PlayerPOV -> screen
