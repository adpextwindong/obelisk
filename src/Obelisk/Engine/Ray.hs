{-# LANGUAGE TupleSections #-}
module Obelisk.Engine.Ray (shootRay', xRayGridIntersections, yRayGridIntersections, baseStepsBounded, visitedPositions,sampleWalkRayPaths) where

import Linear.V2
import Linear.Vector ( (*^), (^*) )
import Linear.Metric ( qd, normalize, norm, distance)
import Debug.Trace (trace)

import Control.Lens ( (^.) )
import qualified Data.Set as S
import Foreign.C.Types ( CInt )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Bifunctor ( Bifunctor(first) )

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.STRef
import Control.Monad.ST

import Obelisk.State
    ( checkAt,
      inBounds,
      accessMapV,
      Vars,
      PVars(camera_plane, direction, position),
      WorldTiles(worldSize) )
import Obelisk.Types.Wall ( WallType(FW) )
import Obelisk.Math.Vector (cosThetaBetween)
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

--NOTE: These rays should be normalized
--TODO newtype for normalized ray invariant
--TODO tests that these are on gridlines
xRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
xRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
    where
        firstStep = abs $ deltaFirst (p^._x) (nr ^._x)
        stepScales = [(firstStep + x) / abs (nr ^._x) | x <- bss]

--NOTE: These rays should be normalized
yRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
yRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
    where
        firstStep = abs $ deltaFirst (p^._y) (nr ^._y)
        stepScales = [(firstStep + y) / abs (nr ^._y) | y <- bss]

{-# INLINE deltaFirst #-}
deltaFirst :: Float -> Float -> Float
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px



epsilon = 0.00001 --TODO maybe lift this to the math module

--This function is for bumping the grid index into the right spot. I have to read physically based rendering some more.
--Some of the math in computing the stepscales leads to values like 28.999999999999996 (when it should be 29 in terms of integers).
--To make sure it checks index 29 which it is close enough to with an epsilon test we add epsilon in the direction of the ray so it truncates properly.
--TODO double check Round in GHC.Real
{-# INLINE epsilonBump #-}
epsilonBump :: V2 Float -> V2 Float -> (V2 Float, V2 Int)
epsilonBump ray result = (result,bumped)
    where
        bumped = V2 x y
        x = truncate $ result ^._x + (epsilon * signum ray ^._x) --Is this neccessary?
        y = truncate $ result ^._y + (epsilon * signum ray ^._y)

mergeIntersections :: V2 Float -> [V2 Float] -> [V2 Float] -> [V2 Float]
mergeIntersections playerpos (x:xs) (y:ys) = if qd playerpos x < qd playerpos y
                                             then x : mergeIntersections playerpos xs (y:ys)
                                             else y : mergeIntersections playerpos (x:xs) ys
mergeIntersections _ [] ys = ys
mergeIntersections _ xs [] = xs

--Samples the raypath for the first wall intersection and returns its position in world space and tile indexes
sampleWalkRayPaths :: WorldTiles -> V2 Float -> V2 Float -> [V2 Float] -> Maybe (V2 Float, V2 Int)
sampleWalkRayPaths _ _ _ [] = Nothing --TODO it might be easier to replace this with a default wall
-- sampleWalkRayPaths world playerpos ray (step:path) | trace ("Ray " ++ show ray ++  " step " ++ show step ++ " epsilonBump " ++ show (epsilonBump ray step)) False = undefined --Trace trick
sampleWalkRayPaths world playerpos ray (step:path) = if accessMapV world checkInds == FW
                                                     then Just cPair
                                                     else sampleWalkRayPaths world playerpos ray path
    where
        --TODO we should distinguish ray with a newtype
        cPair@(checkSpot,checkInds) = epsilonBump ray step

--TODO port sampleWalkRayPaths to a version that uses a mutable vector for visited tiles
stWalkRayPathForWall :: WorldTiles -> V2 Float -> V2 Float -> [(V2 Float, V2 Int)]
  -> (Maybe (V2 Float, V2 Int), U.Vector Bool)
stWalkRayPathForWall w p r path = runST( do
  v <- newSTRef $ U.replicate 100 False
  let wallHit = Nothing
  fmap (wallHit,) $ readSTRef v)

--Raycasts and returns the final location of the world. Samples the world as it walks to prevent building a huge list
--Building the vision set might be a waste of time. Considering we could 
rayCast' :: WorldTiles -> V2 Float -> V2 Float -> Maybe (V2 Float, V2 Int)
rayCast' world p r = sampleWalkRayPaths world p r (mergeIntersections p vints hints)
    where
        vints = xRayGridIntersections p r (baseStepsBounded (fromIntegral $ worldSize world) (p ^._x) (r ^._x))
        hints = yRayGridIntersections p r (baseStepsBounded (fromIntegral $ worldSize world) (p ^._y) (r ^._y))



--TODO fix cameraPlaneSweep so it uniformly gives back n elements spaced across -1 to 1 inclusive
cameraPlaneSweep :: Int -> [Float]
cameraPlaneSweep screenWidth = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0 .. fromIntegral screenWidth - 1]]

--The ray and its angle for fixing fish eye
createRayHead :: V2 Float -> V2 Float -> Float -> (V2 Float, Float)
createRayHead pdir cplane x = (ray, cosThetaBetween ray pdir)
    where ray = normalize (pdir + cplane ^* x)

rayHeads :: CInt -> PVars -> [(V2 Float, Float)]
rayHeads screenWidth player = createRayHead (direction player) (camera_plane player) <$> cameraPlaneSweep (fromIntegral screenWidth)

--Returns the final sample location of the rays
rayCastScreen :: CInt -> PVars -> WorldTiles -> [Maybe (Float, V2 Int)]
rayCastScreen screenWidth player world = rayCastResults
    where
        rayAnglePairs = rayHeads screenWidth player
        rays = fmap fst rayAnglePairs
        rayCastSamples = rayCast' world (position player) <$> rays
        angles = fmap snd rayAnglePairs
        rayCastResults = (\(res, angle) -> fmap (first (applyPermadi (position player) angle)) res) <$> zip rayCastSamples angles

rayCastScreenWithVision :: CInt -> PVars -> WorldTiles -> ([Maybe (V2 Float, V2 Int)], IntMap Bool)
rayCastScreenWithVision screenWidth player world = undefined
    where
        rays = rayHeads screenWidth player

--Permadi Fisheye Fix
applyPermadi :: V2 Float -> Float -> V2 Float -> Float
applyPermadi ppos costheta sampledpos = costheta * distance ppos sampledpos

--Permadi Wall Height
--Make sure this distance is properly projected to prevent fisheye
--NOTE: The direction vector and the camera plane vector length ratios will determine FOV with this impl
projectedSliceHeight :: Float -> PVars -> Float
projectedSliceHeight distanceToSlice player = (sliceConstant / distanceToSlice) * norm (direction player)
    where sliceConstant = 60.0 :: Float --TODO figure this constant out
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

stVisitedPositions :: Vars -> [[(V2 Float, V2 Int)]] -> UM.MVector () Bool
stVisitedPositions = undefined --TODO ST visited positions for list of raypaths

lenPassthrough :: [WallType] -> Int
lenPassthrough = length . takeWhile (/= FW)

--TODO tests
--Samples walls for their contents while within the bounds of the world
wallSamples :: Vars -> [V2 Int] -> [WallType]
wallSamples gs [] = []
wallSamples gs (r:rs) = if inBounds gs r
                        then checkAt gs r : wallSamples gs rs
                        else []

-- HASDEMO: mouseLookRayCastGraphicM
-- Generates a bounded ray path, its vertical intersections with the grid, and horizontal intersections
-- (Path, Vertical Intersections, Horizontal Intersections)
shootRay' :: Int -> V2 Float -> V2 Float -> ([(V2 Float, V2 Int)], [V2 Float], [V2 Float])
shootRay' ws playerpos direction = (epsilonBump direction <$> mergeIntersections playerpos vints hints, vints, hints)
    where
        stepsX = baseStepsBounded ws (playerpos ^._x) (direction ^._x)
        stepsY = baseStepsBounded ws (playerpos ^._y) (direction ^._y)

        boundedHorizontal = takeWhile (\(V2 _ y) -> y > 0 && y < fromIntegral ws)
        boundedVertical = takeWhile (\(V2 x _) -> x > 0 && x < fromIntegral ws)

        vints = if direction ^._x == 0.0
                then [] --No vertical intersection if literally looking along x axis
                else boundedHorizontal $ xRayGridIntersections playerpos direction stepsX
        hints = boundedVertical $ yRayGridIntersections playerpos direction stepsY

--Utilize the new Ray
--TODO make a version that takes in the world so we don't waste time allocing for fat ass lists
--TODO benchmark this

{-
 - TODO UNDO
genRays :: CInt -> PVars -> Int -> [[(V2 Float, V2 Int)]]
genRays screenWidth player worldSize = shootRay' worldSize (position player) <$> rays
    where rayAnglePairs = rayHeads screenWidth player
          rays = fmap fst rayAnglePairs

Lets try to do the rendering pipeline in one step and return all the stuff we'd need in a record

Debug Views expects:

RayPaths
Visited Set

Game Screen expects:

Wall Heights and the respective color/texture sample of the wall
Visited Set for sprite drawing
-}

data RaycastResults
    = RaycastResults {
        
    }

