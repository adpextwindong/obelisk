{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Engine.Ray (Intersection(..), IntAxis(..), rayHeads, shootRay, stScreenWalkRaysForWall) where

import Linear
import Debug.Trace

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
import Control.Monad.State

import Obelisk.State
import Obelisk.Types.Wall
import Obelisk.Math.Vector
import Obelisk.Math.Homogenous
import Obelisk.Graphics.Primitives

import qualified SDL
import Data.Maybe

{-

Given the Player and the Ray, compute the XIntersections along the path of the ray from the player.

From Physically Based Rendering 2.5, Parametric form of the Ray.
r(t) = o + td
In our case o, the ray origin is the player position p and the ray direction is r.

StepScales are t values where there is a grid intersection.
This code assumes walls are space 1.0f apart. Abs for firstStep ensures negative direction intersections are found correctly.

-}
{-# INLINE baseSteps #-}
baseSteps :: [Float]
baseSteps = [0.0 ..]

{-# INLINE upperBound #-}
upperBound :: Int -> Float -> Float -> Int
upperBound worldSize axisPosition axisRay = if axisRay > 0
                                            then floor $ fromIntegral worldSize - axisPosition
                                            else floor axisPosition


--Epsilon is added to prevent stepscales that should be 29 ending up at values like 28.999999999999996
--This prevents the truncated value ending up in the wrong tile index
{-# INLINE xRayGridIntersections #-}
xRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
xRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
    where
        firstStep = abs $ deltaFirst (p^._x) (nr ^._x)
        stepScales = [(firstStep + x + epsilon) / abs (nr ^._x) | x <- bss]

{-# INLINE yRayGridIntersections #-}
yRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
yRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
    where
        firstStep = abs $ deltaFirst (p^._y) (nr ^._y)
        stepScales = [(firstStep + y + epsilon) / abs (nr ^._y) | y <- bss]

{-# INLINE deltaFirst #-}
deltaFirst :: Float -> Float -> Float
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral ((floor px) + 1) - px

epsilon = 0.00001

{-# INLINE verticalIntersection #-}
verticalIntersection :: V2 Float -> Intersection
verticalIntersection x = (uncurry Intersection (posAndInd x)) Vertical

{-# INLINE horizontalIntersection #-}
horizontalIntersection :: V2 Float -> Intersection
horizontalIntersection x = (uncurry Intersection (posAndInd x)) Horizontal

{-# INLINE mergeIntersections #-}
mergeIntersections :: V2 Float -> [V2 Float] -> [V2 Float] -> [Intersection]
mergeIntersections playerpos v@(x:xs) h@(y:ys) = if qd playerpos x < qd playerpos y
                                                 then (verticalIntersection x) : mergeIntersections playerpos xs (y:ys)
                                                 else (horizontalIntersection y) : mergeIntersections playerpos (x:xs) ys
mergeIntersections _ [] ys = fmap horizontalIntersection ys
mergeIntersections _ xs [] = fmap verticalIntersection xs

--Samples the raypath for the first wall intersection and returns its position in world space and tile indexes
{-# INLINE sampleWalkRayPaths #-}
sampleWalkRayPaths :: WorldTiles -> V2 Float -> V2 Float -> [V2 Float] -> Maybe (V2 Float, V2 Int)
sampleWalkRayPaths _ _ _ [] = Nothing
sampleWalkRayPaths world playerpos ray (step:path) = case accessMapV world checkInds of
                                                      FW _ -> Just cPair
                                                      _ -> sampleWalkRayPaths world playerpos ray path
    where
        cPair@(_,checkInds) = posAndInd step

{-# INLINE posAndInd #-}
posAndInd :: V2 Float -> (V2 Float, V2 Int)
posAndInd result = (result, fmap truncate result)

--Walks a lists of ray paths and collect their wall hits into a single array
{-# INLINE stScreenWalkRaysForWall #-}
stScreenWalkRaysForWall :: WorldTiles -> V2 Float -> [[Intersection]] -> ([[(Intersection,Transparency)]], UArray (V2 Int) Bool)
stScreenWalkRaysForWall w p paths = runST aux
  where
    aux :: ST s ([[(Intersection,Transparency)]], UArray (V2 Int) Bool)
    aux = do
      let tileCount = fromIntegral $ worldSize w * worldSize w

      visited <- newArray (0, tileCount) False :: ST s (STUArray s (V2 Int) Bool)

      let go (sIntersection@(Intersection step_position step_inds _) : path) = do
           writeArray visited step_inds True
           case accessMapV w step_inds of
            FW _ -> return $ [(sIntersection,NoTransparency)]
            (TransparentWall t) -> do
              rest <- go path
              return $ (sIntersection, t) : rest
            _ -> go path
          go [] = return []

      results <- mapM go paths
      rv <- freeze visited

      return (results, rv)


{-# INLINE cameraPlaneSweep #-}
--TODO fix cameraPlaneSweep so it uniformly gives back n elements spaced across -1 to 1 inclusive
cameraPlaneSweep :: Int -> [Float]
cameraPlaneSweep screenWidth = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0 .. fromIntegral screenWidth - 1]]

--The ray and its angle for fixing fish eye
{-# INLINE createRayHead #-}
createRayHead :: V2 Float -> V2 Float -> Float -> (V2 Float, Float)
createRayHead pdir cplane x = (ray, cosThetaBetween ray pdir)
    where ray = normalize (pdir - cplane ^* x)

{-# INLINE baseStepsBounded #-}
baseStepsBounded :: Int -> Float -> Float -> [Float]
baseStepsBounded worldSize axisPosition axisRay = take (upperBound worldSize axisPosition axisRay) baseSteps

{-# INLINE boundedHorizontal #-}
boundedHorizontal ws = takeWhile (\(V2 _ y) -> y > 0 && y < fromIntegral ws)
{-# INLINE boundedVertical #-}
boundedVertical ws = takeWhile (\(V2 x _) -> x > 0 && x < fromIntegral ws)

--TODO pipe IntAxis to instruct wall texturing about which offset to use
data Intersection = Intersection (V2 Float) (V2 Int) IntAxis
  deriving Show

data IntAxis = Vertical | Horizontal
  deriving Show

-- HASDEMO: mouseLookRayCastGraphicM
-- Generates a bounded ray path, its vertical intersections with the grid, and horizontal intersections
-- (Intersection Path, Vertical Intersections, Horizontal Intersections)
shootRay :: Int -> V2 Float -> V2 Float -> ([Intersection], [V2 Float], [V2 Float])
shootRay ws playerpos direction = (mergeIntersections playerpos vints hints, vints, hints)
    where
        stepsX = baseStepsBounded ws (playerpos ^._x) (direction ^._x)
        stepsY = baseStepsBounded ws (playerpos ^._y) (direction ^._y)

        vints = if direction ^._x == 0.0
                then [] --No vertical intersection if literally looking along x axis
                else boundedHorizontal ws $ xRayGridIntersections playerpos direction stepsX

        hints = boundedVertical ws $ yRayGridIntersections playerpos direction stepsY

--Returns the rays and its angles for the whole screen
rayHeads :: Int -> PVars -> [(V2 Float, Float)]
rayHeads screenWidth player = createRayHead (direction player) (camera_plane player) <$> cameraPlaneSweep screenWidth
