{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Effect.Renderer where

import Control.Monad.Reader
import qualified SDL
import SDL (($=))
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
    renderSky :: m (Graphic Float)
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

--divide this by 2pi to get an index from 0 to 1
quadrantAngle :: V2 Float -> Float
quadrantAngle (V2 x y) | y > 0 = atan2 y x
                       | otherwise = (2 * pi) + atan2 y x

renderSky' :: (Debug m, SDLCanDraw m, MonadState Vars m) => m (Graphic Float)
renderSky' = do
    window <- asks cWindow

    pdir <- direction . player <$> get
    let ray = normalize pdir

    skyT <- asks cSkyText

    let textureWidth = 1280
        skyWidth = 640
        skyHeight = 240

    let index = quadrantAngle ray / (2 * pi)
    --dprint (show ray ++ " " ++ show index)

    let firstSkyIndex = index * skyWidth :: Float
        secondSkyWidth = (1 - index) * skyWidth :: Float

    dprint firstSkyIndex

    --TODO fix seam split rendering
    --TODO make sky texture 4x the screen width
    return $ GroupPrim  "Sky" [
                              --src
        Prim (CopyRect skyT (V2 (floor firstSkyIndex) 0) --Indexed start
                            (V2 (floor skyWidth) (floor skyHeight)) --Size of sky chunk
                              --Target
                             (V2 0 0)
                             (V2 skyWidth skyHeight)
                             SDL.BlendNone)


        ]

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

pdToWorldT :: M_World2PhysicalDevice Float -> M_PhysicalDevice2World Float
pdToWorldT t = coerce @(M22Affine Float) (inv33 @_ (coerce t))

pdToWorldPos :: M_World2PhysicalDevice Float -> SDL.Point V2 Float -> V2 Float
pdToWorldPos t' (SDL.P pos) = dropHomoCoords . (unTagged (pdToWorldT t') !*) . homoCoords $ pos

rawPDtoWorldPos :: M22Affine Float -> SDL.Point V2 Float -> V2 Float
rawPDtoWorldPos t (SDL.P pos) = dropHomoCoords . (inv33 t !*) . homoCoords $ pos

--------------------------------------------------------------------------------

-- | Maps eval'd primitives to their SDLCanDraw draw calls
drawGraphic :: (Debug m, MonadIO m, SDLCanDraw m) => Graphic CInt -> m ()
drawGraphic (EvaldGP _ evald_xs) = mapM_ drawGraphic evald_xs
drawGraphic (EvaldP (Line start end color))           = (\sr -> drawLine sr start end color) =<< asks cRenderer
drawGraphic (EvaldP (Circle center radius color))     = (\sr -> circle sr center radius color) =<< asks cRenderer
drawGraphic (EvaldP (FillTriangle v0 v1 v2 color))    = (\sr -> fillTriangle sr v0 v1 v2 color) =<< asks cRenderer
drawGraphic (EvaldP (FillRectangle v0 v1 color))      = (\sr -> fillRectangle sr v0 v1 color) =<< asks cRenderer
drawGraphic (EvaldP (FillCircle center radius color)) = (\sr -> fillCircle sr center radius color) =<< asks cRenderer
drawGraphic (EvaldP cr@(CopyRect texture srcStart size dstart dend blendMode)) = do
  let srcRect = SDL.Rectangle (SDL.P srcStart) size
  let dstRect = SDL.Rectangle (SDL.P dstart) (dend - dstart)
  renderer <- asks cRenderer

  SDL.rendererDrawBlendMode renderer $= blendMode
  SDL.copy renderer texture (Just srcRect) (Just dstRect)

drawGraphic (AffineT _ _) = undefined

centerScreenOnWorldGrid :: CInt -> CInt -> CInt -> M_World2PhysicalDevice Float
centerScreenOnWorldGrid = coerce . rawCenterScreenOnWorldGrid

--TODO expose visibility set
raycast :: (SDLCanDraw m, MonadState Vars m) => V2 Float -> m (Graphic Float)
raycast lookingAtWorldPos = do
  let rayCount = 320 -- TODO float out

  pp <- player <$> get
  p <- position . player <$> get
  w <- world <$> get

  let fst3 (a,b,c) = a

  let ws = worldSize w
      rayAnglePairs = rayHeads rayCount pp
      rays = fmap fst rayAnglePairs
      angles = fmap snd rayAnglePairs

      paths = fst3 . shootRay (fromIntegral ws) p <$> rays
      (wallPoints, visitedV) = stScreenWalkRaysForWall w p paths

  -- Screen Graphic
  walls <- mapM (drawWall rayCount p w) $ zip3 wallPoints [0..] angles

  return $ GroupPrim "PlayerPOV" $ concat walls

drawWall :: (SDLCanDraw m, MonadState Vars m) => Int -> V2 Float -> WorldTiles -> ([(Intersection, SDL.BlendMode)], Float, Float) -> m [(Graphic Float)]
drawWall _ _ _ ([], _, _) = return []
drawWall rayCount p w ((((Intersection intpos@(V2 x y) intindex intType), transparency) :xs), rayIndex, rayAngle) = do
    projType <- projectionType . config <$> get
    t <- asks cTextures
    let screenWidth = 640
    let screenHeight = 480
    let screenMiddle = fromIntegral screenHeight / 2
    let wallWidth = fromIntegral $ screenWidth `div` fromIntegral rayCount

    let wallHeight = 64 --TODO Tune
        distanceToSlice = case projType of
                            FishEye -> norm $ intpos - p
                            Permadi -> rayAngle * distance p intpos
        projectedWallHeight = wallHeight / distanceToSlice
        wallTop    = screenMiddle - projectedWallHeight
        wallBottom = screenMiddle + projectedWallHeight
        wallLeft   = rayIndex * fromIntegral wallWidth
        wallRight  = (rayIndex + 1) * fromIntegral wallWidth

        --Texture Index
        fullWallTexInd (FW i _) = i
        textureIndex = fromIntegral $ fullWallTexInd $ accessMapV w intindex
        --Texutre Mapping
        textureOffset x size = truncate $ (64 * textureIndex) + size * (x - (fromIntegral . truncate $ x))
        textureChunk = case intType of
                          Vertical -> textureOffset y 64
                          Horizontal -> textureOffset x 64

    rest <- drawWall rayCount p w (xs, rayIndex, rayAngle)

    --TODO plumb transparency
    let currWall = Prim $ CopyRect (fromJust t) (V2 textureChunk 0) (V2 1 64) (V2 wallLeft wallTop) (V2 wallRight wallBottom) transparency

    return $ currWall : rest



--
-- DEMOS
--

--raycast at mouse look demo : grenderMouseLook mouseLookRaycastGraphicM
mouseLookRaycastGraphicM :: (SDLCanDraw m, Debug m, MonadState Vars m) => m (Graphic Float)
mouseLookRaycastGraphicM  = do
    pdir <- fmap normalize $ direction . player <$> get
    let ray = pdir
    screen <- raycast pdir

    --TODO undisable after plumbing for transparent wall drawing is done
    p <- position . player <$> get
    w <- world <$> get
    let ws = worldSize w
    camZoom <- camZoomScale <$> get

    --let ray = normalize $ lookingAtWorldPos - p

    pp <- player <$> get
    let mousePlayer = pp {
      direction = ray,
      camera_plane = normalize $ ray *! rotation2 (pi / 2.0)
    }

    let circleAt color c = Prim $ Circle c (floor camZoom) color
    let fst3 (a,b,c) = a
    let tempRayCount = 320 :: Int
    let rayAnglePairs = rayHeads tempRayCount mousePlayer :: [(V2 Float, Float)]
    let rays = fmap fst rayAnglePairs
    let rayAngles = fmap snd rayAnglePairs

    let paths = fst3 . shootRay (fromIntegral ws) p <$> rays :: [[Intersection]]
    let (wallPoints, visitedV) = stScreenWalkRaysForWall w p paths

    let rayCastPoints = fmap ((\((Intersection c _ _),_) -> circleAt yellow c)) <$> wallPoints

    let rayCastPointsG = GroupPrim "16 ScreenWidth Raycast Points" $ concat rayCastPoints

    --Single ray path and intersections
    let (_singlePath, vints, hints) = shootRay (fromIntegral ws) p ray

    -- Overhead field of view done with triangles of adjacent intersections and the player as tri verts
    let triangleAt a b c = Prim $ FillTriangle a b c yellow
    let rayIntersections = fmap (\((Intersection p _ _),_) -> p) $ concat wallPoints

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
