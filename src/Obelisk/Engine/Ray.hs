module Obelisk.Engine.Ray where

import Linear
import Linear.V2
import Linear.Vector
import Linear.Metric
import Linear.Epsilon
import Linear.Affine

import Control.Lens
import Data.Functor
import qualified Data.Set as S
import Foreign.C.Types

import Obelisk.Graphics.Primitives
import Obelisk.Graphics.DebugUI
import Obelisk.State (emptyMap)
import SDL.Primitive (horizontalLine)
import Obelisk.Math.Homogenous

--Positions in world space the raycaster will actually being checking
type DDAStep = V2 Double

{-

Given the Player and the Ray, compute the XIntersections along the path of the ray from the player.

From Physically Based Rendering 2.5, Parametric form of the Ray.
r(t) = o + td
In our case o, the ray origin is the player position p and the ray direction is r.

StepScales are t values where there is a grid intersection.
This code assumes walls are space 1.0f apart. Abs for firstStep ensures negative direction intersections are found correctly.

-}
xRayGridIntersections :: V2 Double -> V2 Double -> [V2 Double]
xRayGridIntersections p r = ((p +) . (*^ nr)) <$> stepScales
    where
        nr = normalize r
        firstStep = abs $ deltaFirst (p^._x) (nr ^._x)
        stepScales = [(firstStep + x) / (abs (nr ^._x)) | x <- [0.0 .. 10.0]] --TODO unbound this once everything is kosher so it can scale to any worldsize


yRayGridIntersections :: V2 Double -> V2 Double -> [V2 Double]
yRayGridIntersections p r = ((p +) . (*^ nr)) <$> stepScales
    where
        nr = normalize r
        firstStep = abs $ deltaFirst (p^._y) (nr ^._y)
        stepScales = [(firstStep + y) / (abs (nr ^._y)) | y <- [0.0 .. 10.0]] --TODO unbound this once everythign is kosher so it can scale to any worldsize

deltaFirst :: Double -> Double -> Double 
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px

--Lets test the ray regularly without concerning ourselves with the bumping into correct tile for wall positions. Maybe an epsilon aware rounder could work.
naivePath :: Int -> V2 Double -> V2 Double -> [(V2 Double, V2 Int)]
naivePath ws player direction = (epsilonBump direction) <$> mergeIntersections player vints hints
    where
        vints = clipWorld (fromIntegral ws) $ xRayGridIntersections player direction
        hints = clipWorld (fromIntegral ws) $ yRayGridIntersections player direction

--NOTE, watch out it nXForm currently spits out values like this. We'll need some rounding aware of this, wherever we also use the V2 Double value for distance handling.
--5.000000000000001
--28.999999999999996

epsilon = 0.00001 --TODO maybe lift this to the math module

epsilonBump :: V2 Double -> V2 Double -> (V2 Double, V2 Int)
epsilonBump ray result = (result,bumped)
    where
        bumped = V2 x y
        x = truncate $ result ^._x + (epsilon * signum ray ^._x) --Is this neccessary?
        y = truncate $ result ^._y + (epsilon * signum ray ^._y)

mergeIntersections :: V2 Double -> [V2 Double] -> [V2 Double] -> [V2 Double]
mergeIntersections player (x:xs) (y:ys) = if distance player x < distance player y
                                          then x : mergeIntersections player xs (y:ys)
                                          else y : mergeIntersections player (x:xs) ys
mergeIntersections _ [] ys = ys
mergeIntersections _ xs [] = xs

--Takes a list while its members are still in the world bounding box
clipWorld :: CInt -> [V2 Double] -> [V2 Double]
clipWorld ws = takeWhile (\v@(V2 x y) -> x <= fromIntegral ws && y <= fromIntegral ws
                                            && x >= 0 && y >= 0)

{-

TEST FIXTURES. `grender tgp` in GHCI to see the testing rig for this code

-}
p = V2 5.25 5.66
-- p = V2 0 0
r = V2 (1.0) (1.0)

worldSize = 10

vints = clipWorld worldSize $ xRayGridIntersections p r
hints = clipWorld worldSize $ yRayGridIntersections p r

path = naivePath (fromIntegral worldSize) p r

wholeFloatE v = (v - fromIntegral (floor v)) < epsilon
v2OR :: V2 Bool -> Bool
v2OR (V2 x y) = x || y
path_test path = or $ fmap (v2OR . (fmap wholeFloatE) . fst) path --This should be true if all either member of the V2 Double of the path is on a gridline.

visitedSet = S.fromList $ fmap snd path

--Test with grender
tgp = anonGP [
    worldGridTilesGraphic emptyMap visitedSet,
    worldGridGraphic worldSize,
    Prim $ Circle p 1 white,
    anonGP $ (\c -> Prim $ Circle c 1 blue) <$> vints,
    anonGP $ (\c -> Prim $ Circle c 1 red) <$> hints]

--TODO determine set of visited voxels