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

import Obelisk.State
import Obelisk.Types.Wall

import Obelisk.Math.Vector
import Obelisk.Math.Homogenous ( rotation, translate )
import Obelisk.Graphics.Primitives
import Obelisk.Engine.Ray (shootRay', xRayGridIntersections, yRayGridIntersections, baseStepsBounded)
import Obelisk.Wrapper.SDLRenderer (SDLRenderer(circle))
import Linear (normalize)
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

worldGridGraphic :: CInt -> Graphic (Shape Float)
worldGridGraphic ws = GroupPrim "Grid Lines" gridLines
    where
        worldSize = fromIntegral ws
        verticalLines ws   = [Prim (Line (V2 x 0) (V2 x ws) gridColor) | x <- [0..ws]]
        horizontalLines ws = [Prim (Line (V2 0 y) (V2 ws y) gridColor) | y <- [0..ws]]
        gridLines = verticalLines worldSize ++ horizontalLines worldSize

worldGridTilesGraphic :: WorldTiles -> S.Set (V2 Int) -> Graphic (Shape Float)
worldGridTilesGraphic world visitedSet = do
    let ws = worldSize world 
    
    let inds = [(x,y) | x <- [0..ws -1], y <- [0..ws - 1]]
    let quads = [(V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) | x <- [0.. fromIntegral ws - 1], y <- [0.. fromIntegral ws - 1]]

    let prims = zip inds quads <&> (\((x,y), (vA,vB,vC,vD)) -> do
            let sampleColor = wallTypeToColor $ accessMap world (fromIntegral x) (fromIntegral y)
            let tileColor = if S.member (V2 (fromIntegral x) (fromIntegral y)) visitedSet
                --Lighten the tiles that get rayCasted
                --TODO this should be a graphic highlight
                then sampleColor + V4 20 20 20 0
                else sampleColor

            [Prim $ FillTriangle vA vB vC tileColor,
             Prim $ FillTriangle vB vC vD tileColor])
    GroupPrim "World Grid Tiles" $ concat prims

playerGraphic :: PVars -> Graphic (Shape Float)
playerGraphic p = GroupPrim "Player Graphic" [
                    playerCircleGraphic p,
                    cameraPlaneGraphic p,
                    playerArrowGraphic p
                ]

playerArrowGraphic :: PVars -> Graphic (Shape Float)
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

cameraPlaneGraphic :: PVars -> Graphic (Shape Float)
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

playerCircleGraphic :: PVars -> Graphic (Shape Float)
playerCircleGraphic p = do
    let px = position p ^._x
    let py = position p ^._y
    let circle_radius = 3
    AffineT (translate px py) $ Prim (Circle (V2 0.0 0.0) circle_radius white)

--Raycasting Diagnostics
--
{-
midlineRaycastIntersectionsGraphic :: PVars -> CInt -> Graphic (Shape Float)
midlineRaycastIntersectionsGraphic player ws = do
    let intersections = fst <$> fshootRay' (fromIntegral ws) (position player) (direction player) :: [V2 Float]
    GroupPrim "Midline Intersections Graphic" $ (\pos -> Prim $ Circle pos 3 yellow) <$> intersections

-}

{-
--Test with grender for Ray
singleRaycastGraphic :: Graphic (Shape Float)
singleRaycastGraphic =
    let p = V2 5.25 5.66
        r = V2 1.0 1.0
        path = fshootRay' 10 p r
        vints = xRayGridIntersections p r $ baseStepsBounded 10 (p ^._x) (r ^._x)
        hints = yRayGridIntersections p r $ baseStepsBounded 10 (p ^._y) (r ^._y)

        visitedSet = S.fromList $ fmap snd path
        in anonGP [
            worldGridTilesGraphic emptyMap visitedSet,
            worldGridGraphic 10, --TODO unbound
            Prim $ Circle p 1 white,
            anonGP $ (\c -> Prim $ Circle c 1 blue) <$> vints,
            anonGP $ (\c -> Prim $ Circle c 1 red) <$> hints]
-}
--raycast at mouse look demo : grenderMouseLook mouseLookRaycastGraphicM
--TODO flexibility to zoom/translate the screen around in the runner for this
mouseLookRaycastGraphicM :: (Debug m, MonadState Vars m) => V2 Float -> m (Graphic (Shape Float))
mouseLookRaycastGraphicM  lookingAtWorldPos = do
    (Vars (PVars p _ _) (WorldTiles _ ws) _ _ camZoom _) <- get
    let
        playerCircle = Prim $ Circle p (floor camZoom) white
        r = normalize $ lookingAtWorldPos - p
        (path, vints, hints) = shootRay' (fromIntegral ws) p r
        visitedSet = S.fromList $ fmap snd path

        circleAt color c = Prim $ Circle c (floor camZoom) color
        --TODO place yellow circles at sample points
        in return $ GroupPrim "MouseLookSingleRayIntersections" [
            worldGridTilesGraphic emptyMap visitedSet,
            worldGridGraphic 10,
            playerCircle,
            GroupPrim "Vertical Intersections" $ (red `circleAt`) <$> vints,
            GroupPrim "Horizontal Intersections" $ (blue `circleAt`) <$> hints
            ]
