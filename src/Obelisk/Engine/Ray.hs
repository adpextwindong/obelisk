{-# LANGUAGE MagicHash #-}
module Obelisk.Engine.Ray where

import Linear.V2
import Linear.Vector ( (*^), (^*) )
import Linear.Metric ( Metric(distance), normalize )
import Debug.Trace (trace)

import Control.Lens ( (^.) )
import qualified Data.Set as S
import Foreign.C.Types ( CInt )

import Obelisk.State
import Obelisk.Types.Wall
-- import Obelisk.Graphics.DebugUI
import Obelisk.State (emptyMap)
import SDL.Primitive (horizontalLine)

--Positions in world space the raycaster will actually being checking
type DDAStep = V2 Float

{-

Given the Player and the Ray, compute the XIntersections along the path of the ray from the player.

From Physically Based Rendering 2.5, Parametric form of the Ray.
r(t) = o + td
In our case o, the ray origin is the player position p and the ray direction is r.

StepScales are t values where there is a grid intersection.
This code assumes walls are space 1.0f apart. Abs for firstStep ensures negative direction intersections are found correctly.

-}
baseSteps :: [Float]
baseSteps = [0.0 ..]

upperBound :: Int -> Float -> Float -> Int
upperBound worldSize axisPosition axisRay = if axisRay > 0
                                            then floor $ fromIntegral worldSize - axisPosition
                                            else floor axisPosition

baseStepsBounded :: Int -> Float -> Float -> [Float]
baseStepsBounded worldSize axisPosition axisRay = take (upperBound worldSize axisPosition axisRay) baseSteps

xRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
xRayGridIntersections p r bss = (p +) . (*^ nr) <$> stepScales
    where
        nr = normalize r
        firstStep = abs $ deltaFirst (p^._x) (nr ^._x)
        stepScales = [(firstStep + x) / abs (nr ^._x) | x <- bss] --TODO unbound this once everything is kosher so it can scale to any worldsize


yRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
yRayGridIntersections p r bss = (p +) . (*^ nr) <$> stepScales
    where
        nr = normalize r
        firstStep = abs $ deltaFirst (p^._y) (nr ^._y)
        stepScales = [(firstStep + y) / abs (nr ^._y) | y <- bss] --TODO unbound this once everythign is kosher so it can scale to any worldsize

deltaFirst :: Float -> Float -> Float
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px

--Lets test the ray regularly without concerning ourselves with the bumping into correct tile for wall positions. Maybe an epsilon aware rounder could work.
shootRay' :: Int -> V2 Float -> V2 Float -> [(V2 Float, V2 Int)]
shootRay' ws playerpos direction = epsilonBump direction <$> mergeIntersections playerpos vints hints
    where
        vints = xRayGridIntersections playerpos direction (baseStepsBounded ws (playerpos ^._x) (direction ^._x))
        hints = yRayGridIntersections playerpos direction (baseStepsBounded ws (playerpos ^._y) (direction ^._y))

--NOTE, watch out it nXForm currently spits out values like this. We'll need some rounding aware of this, wherever we also use the V2 Float value for distance handling.
--5.000000000000001
--28.999999999999996

epsilon = 0.00001 --TODO maybe lift this to the math module

epsilonBump :: V2 Float -> V2 Float -> (V2 Float, V2 Int)
epsilonBump ray result = (result,bumped)
    where
        bumped = V2 x y
        x = truncate $ result ^._x + (epsilon * signum ray ^._x) --Is this neccessary?
        y = truncate $ result ^._y + (epsilon * signum ray ^._y)

mergeIntersections :: V2 Float -> [V2 Float] -> [V2 Float] -> [V2 Float]
mergeIntersections playerpos (x:xs) (y:ys) = if distance playerpos x < distance playerpos y
                                          then x : mergeIntersections playerpos xs (y:ys)
                                          else y : mergeIntersections playerpos (x:xs) ys
mergeIntersections _ [] ys = ys
mergeIntersections _ xs [] = xs

sampleWalkRayPaths :: WorldTiles -> V2 Float -> V2 Float -> [V2 Float] -> Maybe (V2 Float, V2 Int)
sampleWalkRayPaths _ _ _ [] = Nothing
-- sampleWalkRayPaths world playerpos ray (step:path) | trace ("Ray " ++ show ray ++  " step " ++ show step ++ " epsilonBump " ++ show (epsilonBump ray step)) False = undefined --Trace trick
sampleWalkRayPaths world playerpos ray (step:path) = if accessMapV world checkInds == FW
                                                     then Just cPair
                                                     else sampleWalkRayPaths world playerpos ray path
    where
        --TODO we should distinguish ray with a newtype
        cPair@(checkSpot,checkInds) = epsilonBump ray step

--Raycasts and returns the final location of the world. Samples the world as it walks to prevent building a huge list
--Building the vision set might be a waste of time. Considering we could 
rayCast' :: WorldTiles -> V2 Float -> V2 Float -> Maybe (V2 Float, V2 Int)
rayCast' world p r = sampleWalkRayPaths world p r (mergeIntersections p vints hints)
    where
        vints = xRayGridIntersections p r (baseStepsBounded (fromIntegral $ worldSize world) (p ^._x) (r ^._x))
        hints = yRayGridIntersections p r (baseStepsBounded (fromIntegral $ worldSize world) (p ^._y) (r ^._y))

--Takes a list while its members are still in the world bounding box
-- clipWorld :: CInt -> [V2 Float] -> [V2 Float]
-- clipWorld ws = takeWhile (\v@(V2 x y) -> x <= fromIntegral ws && y <= fromIntegral ws
--                                             && x >= 0 && y >= 0)

{-

TEST FIXTURES. `grender tgp` in GHCI to see the testing rig for this code

-}
-- p = V2 0 0

-- wholeFloatE v = (v - fromIntegral (floor v)) < epsilon
-- v2OR :: V2 Bool -> Bool
-- v2OR (V2 x y) = x || y
-- path_test path = or $ fmap (v2OR . (fmap wholeFloatE) . fst) path --This should be true if all either member of the V2 Float of the path is on a gridline.

--TODO determine set of visited voxels

--Utilize the new Ray
--TODO make a version that takes in the world so we don't waste time allocing for fat ass lists
--TODO benchmark this
genRays :: CInt -> PVars -> Int -> [[(V2 Float, V2 Int)]]
genRays screenWidth player worldSize = shootRay' worldSize (position player) <$> rayHeads screenWidth player

--TODO fix cameraPlaneSweep so it uniformly gives back n elements spaced across -1 to 1 inclusive
cameraPlaneSweep screenWidth = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0 .. fromIntegral screenWidth - 1]]

rayHeads :: CInt -> PVars -> [V2 Float]
rayHeads screenWidth player = [direction player + (camera_plane player ^* x) | x <- cameraPlaneSweep screenWidth] :: [V2 Float]
--Returns the final sample location of the rays
rayCastScreen :: CInt -> PVars -> WorldTiles -> [Maybe (V2 Float, V2 Int)]
rayCastScreen screenWidth player world = rayCast' world (position player) <$> rayHeads screenWidth player


{-
Note:

Buidling up this pathlist and unioning the set is a lot of work. We could just sample the wallhit.

If we really need a visited set for debug/sprite visibility we can consider making a 2D BitVector in ST and setting it true as we walk through.

TODO ST Ref rayCastWithVis
-}

visitedPositions :: Vars -> [(V2 Float, V2 Int)] -> S.Set (V2 Int)
visitedPositions gs steps = S.fromList $ take takeLength rayVisitedIndexes
    where
        rayVisitedIndexes = fmap snd steps
        takeLength = lenPassthrough (wallSamples gs rayVisitedIndexes)

lenPassthrough :: [WallType] -> Int
lenPassthrough = length . takeWhile (/= FW)

wallSamples :: Vars -> [V2 Int] -> [WallType]
wallSamples gs [] = []
wallSamples gs (r:rs) = if inBounds gs r
                        then checkAt gs r : wallSamples gs rs
                        else []