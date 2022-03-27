{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Engine.Ray (rayHeads, shootRay, stScreenWalkRaysForWall, stWalkRayPathForWall) where

import Linear.V2
import Linear.Vector ( (*^), (^*) )
import Linear.Metric ( qd, normalize, norm, distance)
import Debug.Trace (trace)

import Control.Lens ( (^.) )
import qualified Data.Set as S
import Foreign.C.Types ( CInt )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Bifunctor

import Data.STRef
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed
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

--Epsilon is added to prevent stepscales that should be 29 ending up at values like 28.999999999999996
--This prevents the truncated value ending up in the wrong tile index
xRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
xRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
    where
        firstStep = abs $ deltaFirst (p^._x) (nr ^._x)
        stepScales = [(firstStep + x + epsilon) / abs (nr ^._x) | x <- bss]

yRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
yRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
    where
        firstStep = abs $ deltaFirst (p^._y) (nr ^._y)
        stepScales = [(firstStep + y + epsilon) / abs (nr ^._y) | y <- bss]

{-# INLINE deltaFirst #-}
deltaFirst :: Float -> Float -> Float
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px

epsilon = 0.00001

mergeIntersections :: V2 Float -> [V2 Float] -> [V2 Float] -> [V2 Float]
mergeIntersections playerpos (x:xs) (y:ys) = if qd playerpos x < qd playerpos y
                                             then x : mergeIntersections playerpos xs (y:ys)
                                             else y : mergeIntersections playerpos (x:xs) ys
mergeIntersections _ [] ys = ys
mergeIntersections _ xs [] = xs

--Samples the raypath for the first wall intersection and returns its position in world space and tile indexes
sampleWalkRayPaths :: WorldTiles -> V2 Float -> V2 Float -> [V2 Float] -> Maybe (V2 Float, V2 Int)
sampleWalkRayPaths _ _ _ [] = Nothing
sampleWalkRayPaths world playerpos ray (step:path) = if accessMapV world checkInds == FW
                                                     then Just cPair
                                                     else sampleWalkRayPaths world playerpos ray path
    where
        cPair@(_,checkInds) = noEpsilonBump ray step

noEpsilonBump :: V2 Float -> V2 Float -> (V2 Float, V2 Int)
noEpsilonBump _ result = (result, floored)
  where
    floored = fmap truncate result

--Walks a single ray path until it hits a wall
stWalkRayPathForWall :: WorldTiles -> V2 Float -> [(V2 Float, V2 Int)]
  -> (Maybe (V2 Float, V2 Int), UArray (V2 Int) Bool)

stWalkRayPathForWall w p path = runST aux
  where
    aux :: ST s (Maybe (V2 Float, V2 Int), UArray (V2 Int) Bool)
    aux = do
      let tileCount = fromIntegral $ worldSize w * worldSize w

      visited <- newArray (0, tileCount) False :: ST s (STUArray s (V2 Int) Bool)
      wallHit <- newSTRef Nothing

      let go (sPair@(step_position, step_inds) : path) = do
           writeArray visited step_inds True
           if accessMapV w step_inds == FW
           then do
            writeSTRef wallHit (Just sPair)
           else go path
          go [] = return ()

      go path

      rw <- readSTRef wallHit
      rv <- freeze visited
      return (rw,rv)

--Walks a lists of ray paths and collect their wall hits into a single array
stScreenWalkRaysForWall :: WorldTiles -> V2 Float -> [[(V2 Float, V2 Int)]] -> ([Maybe (V2 Float, V2 Int)], UArray (V2 Int) Bool)
stScreenWalkRaysForWall w p paths = runST aux
  where
    aux :: ST s ([Maybe (V2 Float, V2 Int)], UArray (V2 Int) Bool)
    aux = do
      let tileCount = fromIntegral $ worldSize w * worldSize w

      visited <- newArray (0, tileCount) False :: ST s (STUArray s (V2 Int) Bool)

      let go (sPair@(step_position, step_inds) : path) = do
           writeArray visited step_inds True
           if accessMapV w step_inds == FW
           then do
            return $ Just sPair
           else go path
          go [] = return Nothing

      results <- mapM go paths
      rv <- freeze visited

      return (results, rv)

--TODO fix cameraPlaneSweep so it uniformly gives back n elements spaced across -1 to 1 inclusive
cameraPlaneSweep :: Int -> [Float]
cameraPlaneSweep screenWidth = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0 .. fromIntegral screenWidth - 1]]

--The ray and its angle for fixing fish eye
createRayHead :: V2 Float -> V2 Float -> Float -> (V2 Float, Float)
createRayHead pdir cplane x = (ray, cosThetaBetween ray pdir)
    where ray = normalize (pdir - cplane ^* x)

-- HASDEMO: mouseLookRayCastGraphicM
-- Generates a bounded ray path, its vertical intersections with the grid, and horizontal intersections
-- (Path, Vertical Intersections, Horizontal Intersections)
shootRay :: Int -> V2 Float -> V2 Float -> ([(V2 Float, V2 Int)], [V2 Float], [V2 Float])
shootRay ws playerpos direction = (noEpsilonBump direction <$> mergeIntersections playerpos vints hints, vints, hints)
    where
        stepsX = baseStepsBounded ws (playerpos ^._x) (direction ^._x)
        stepsY = baseStepsBounded ws (playerpos ^._y) (direction ^._y)

        --TODO refactor these takeWhile's out
        boundedHorizontal = takeWhile (\(V2 _ y) -> y > 0 && y < fromIntegral ws)
        boundedVertical = takeWhile (\(V2 x _) -> x > 0 && x < fromIntegral ws)

        vints = if direction ^._x == 0.0
                then [] --No vertical intersection if literally looking along x axis
                else boundedHorizontal $ xRayGridIntersections playerpos direction stepsX
        hints = boundedVertical $ yRayGridIntersections playerpos direction stepsY

rayHeads :: Int -> PVars -> [(V2 Float, Float)]
rayHeads screenWidth player = createRayHead (direction player) (camera_plane player) <$> cameraPlaneSweep screenWidth
