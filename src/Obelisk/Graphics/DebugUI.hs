{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Graphics.DebugUI where

import Obelisk.Effect.Debug

import Foreign.C.Types (CInt)
import Linear
import Control.Lens
import qualified SDL
import qualified SDL.Primitive as SDL
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Array.Unboxed

import Obelisk.State
import Obelisk.Types.Wall

import Obelisk.Math.Vector
import Obelisk.Math.Homogenous ( rotation, translate, rotation2 )
import Obelisk.Graphics.Primitives
import Obelisk.Wrapper.SDLRenderer (SDLRenderer(circle))
import Linear (normalize)

import Obelisk.Engine.Ray (rayHeads, shootRay, stScreenWalkRaysForWall, stWalkRayPathForWall, parallelRaycast)
-- UI CONSTANTS
gridColor :: SDL.Color
gridColor = SDL.V4 63 63 63 maxBound
white :: SDL.Color
white = SDL.V4 maxBound maxBound maxBound maxBound
red :: SDL.Color
red = SDL.V4 maxBound 0 0 maxBound
arrowColor :: SDL.Color
arrowColor = SDL.V4 255 51 51 maxBound

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

-- One thing to note about this is that all of this should be done in world coordinates
-- The Grid to player as center local -> screen AFT will be applied as an AffineT in the renderer

worldGridGraphic :: CInt -> Graphic Float
worldGridGraphic ws = GroupPrim "Grid Lines" gridLines
    where
        worldSize = fromIntegral ws
        verticalLines ws   = [Prim (Line (V2 x 0) (V2 x ws) gridColor) | x <- [0..ws]]
        horizontalLines ws = [Prim (Line (V2 0 y) (V2 ws y) gridColor) | y <- [0..ws]]
        gridLines = verticalLines worldSize ++ horizontalLines worldSize

worldGridTilesGraphic :: WorldTiles -> UArray (V2 Int) Bool -> Graphic Float
worldGridTilesGraphic world visitedSet = do
    let ws = worldSize world

    let inds = [(x,y) | x <- [0..ws -1], y <- [0..ws - 1]]
    let quads = [(V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) | x <- [0.. fromIntegral ws - 1], y <- [0.. fromIntegral ws - 1]]

    let prims = zip inds quads <&> (\((x,y), (vA,vB,vC,vD)) -> do
            let sampleColor = wallTypeToColor $ accessMap world (fromIntegral x) (fromIntegral y)
            let tileColor = if visitedSet ! (V2 (fromIntegral x) (fromIntegral y))
                --Lighten the tiles that get rayCasted
                --TODO this should be a graphic highlight
                then sampleColor + V4 20 20 20 0
                else sampleColor

            [Prim $ FillTriangle vA vB vC tileColor,
             Prim $ FillTriangle vB vC vD tileColor])
    GroupPrim "World Grid Tiles" $ concat prims

playerGraphic :: PVars -> Graphic Float
playerGraphic p = GroupPrim "Player Graphic" [
                    playerCircleGraphic p,
                    cameraPlaneGraphic p,
                    playerArrowGraphic p
                ]

playerArrowGraphic :: PVars -> Graphic Float
playerArrowGraphic player = do
    let playerT = translate (position player^._x) (position player^._y)
    let arrowT = playerT !*! rotation (vectorAngle . direction $ player)
                                                --                 |
    --TODO figure out a better way to handle the scaling done here V
    let dir_len = norm $ direction player
    let arrowLine = Prim $ Line (V2 0 0) (V2 10 0) red

    --Draw Arrow
    --TODO assert direction is larger than the arrow length so we dont get a graphical error
    let arrowLength = 0.25
    let arrowWidth = 0.06

    let arrowHead = Prim (FillTriangle
                            (V2 0.0 (-arrowWidth))
                            (V2 arrowLength 0.0)
                            (V2 0.0 arrowWidth)
                            arrowColor)

    let arrowHeadDisplacementT = translate (1.05*dir_len - arrowLength) 0.0

    let arrow = AffineT arrowT $ GroupPrim "Player Arrow" [
                             arrowLine,
                             AffineT arrowHeadDisplacementT arrowHead
                          ]

    arrow

cameraPlaneGraphic :: PVars -> Graphic Float
cameraPlaneGraphic p = do
    let ppos = position p
    let camTail = ppos + direction p - camera_plane p
    let camHead = ppos + direction p + camera_plane p

    let planeLine = Line camTail camHead white

    let edgeLength = 10.0
    let leftEnd = ppos + (edgeLength *^ (direction p - camera_plane p))
    let leftCamEdgeLine = Line ppos leftEnd gridColor

    let rightEnd = ppos + (edgeLength *^ (direction p + camera_plane p))
    let rightCamEdgeLine = Line ppos rightEnd gridColor

    GroupPrim "Camera Graphic" [
        Prim planeLine,
        Prim leftCamEdgeLine,
        Prim rightCamEdgeLine]

playerCircleGraphic :: PVars -> Graphic Float
playerCircleGraphic p = do
    let px = position p ^._x
    let py = position p ^._y
    let circle_radius = 3
    AffineT (translate px py) $ Prim (Circle (V2 0.0 0.0) circle_radius white)

--
-- DEMOS
--

--raycast at mouse look demo : grenderMouseLook mouseLookRaycastGraphicM
mouseLookRaycastGraphicM :: (Debug m, MonadState Vars m) => V2 Float -> m (Graphic Float)
mouseLookRaycastGraphicM  lookingAtWorldPos = do
    screen <- parallelRaycast lookingAtWorldPos
    return screen

    {-


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

    let paths = fst3 . shootRay (fromIntegral ws) p <$> rays :: [[(V2 Float, V2 Int)]]
    let (wallPoints, visitedV) = stScreenWalkRaysForWall w p paths

    let rayCastPoints = fmap (circleAt yellow . fst) <$> wallPoints

    let rayCastPointsG = GroupPrim "16 ScreenWidth Raycast Points" . catMaybes $ rayCastPoints

    --Single ray path and intersections
    let (_singlePath, vints, hints) = shootRay (fromIntegral ws) p ray
    let _visitedSingle = snd $ stWalkRayPathForWall w p _singlePath

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
            --worldGridTilesGraphic w _visitedSingle, --SingleRayIntersection
            worldGridGraphic 10,--TODO ask GVAR for worldsize
            playerCircle,
            GroupPrim "Vertical Intersections" $ (red `circleAt`) <$> vints,
            GroupPrim "Horizontal Intersections" $ (blue `circleAt`) <$> hints,
            rayCastPointsG] -- ++ fieldOfViewTestTris
      --ScreenSpace
      PlayerPOV -> GroupPrim "Player Point of View" screen

-}

oldScreenGraphic :: (MonadState Vars m) => [Maybe (V2 Float, V2 Int)] -> [Float] -> Integer -> CInt -> Int -> m [Graphic Float]
oldScreenGraphic wallPoints angles screenWidth screenHeight rayCount = do
  w <- world <$> get
  p <- player <$> get

  projType <- projectionType . config <$> get

  let wallWidth = fromIntegral $ screenWidth `div` fromIntegral rayCount
  let wallHeight = 64 --Wall Height 64, Player Height 32?
  let screenMiddle = fromIntegral screenHeight / 2
  let wallFromMaybe (mInt,index, rayAngle) = case mInt of
                                    Nothing -> Nothing
                                    Just (intpos, intindex) -> let distanceToSlice = case projType of
                                                                    FishEye -> norm $ intpos - (position p)
                                                                    Permadi -> rayAngle * distance (position p) intpos

                                                                   --TODO distance to the projection plane?
                                                                   --TODO check if the screen is inverted
                                                                   projectedWallHeight = wallHeight / distanceToSlice
                                                                   wallTop = screenMiddle - projectedWallHeight
                                                                   wallBottom = screenMiddle + projectedWallHeight
                                                                   wallLeft = index * wallWidth
                                                                   wallRight = (index + 1) * wallWidth in
                                                               Just $ Prim $ FillRectangle (V2 wallLeft wallTop) (V2 wallRight wallBottom) filledTileColor

  let walls = catMaybes $ wallFromMaybe <$> zip3 wallPoints [0..] angles
  return walls
