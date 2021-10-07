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
$\hat{p} - \Delta\hat{v} = < I , \_>$
Find the integer solutions of this form

px + delta*vx = Ix
delta * vx = Ix - Px
delta = (Ix - Px) / Vx

We compute delta for every i'th intersection on the grid we're looking for and scale r and add it to the player's original position
-}

--Given the Player and the Ray, compute the XIntersections along the path of the ray from the player.
nXForm :: V2 Double -> V2 Double -> [V2 Double]
nXForm p r = ((p +) . (*^ nr)) <$> stepScales
    where
        nr = normalize r
        firstStep = deltaFirst (p^._x) (nr ^._x)
        stepScales = [(firstStep + x) / (abs (nr ^._x)) | x <- [0.0 .. 10.0]]

testNXForm :: [V2 Double] -> [Bool]
testNXForm = \xs -> tpNXForm <$> xs

txs = nXForm (V2 0.3333 0.42) (V2 2 2)
tpNXForm = (\v -> v^._x == fromIntegral (round (v^._x)))

t_xValsChecked = (\v -> v^._x) <$> txs
--NOTE, watch out it nXForm currently spits out values like this. We'll need some rounding aware of this, wherever we also use the V2 Double value for distance handling.
--5.000000000000001
--28.999999999999996

{-TODO maybe Ray should be

data Ray = V2 Double
         | YAxis
         | XAxis

This way we can nicely handle X = 0, Y = 0 just incase.
-}

--TODO test with different rays and positions.
--TODO test that nYForm and nXForm gives intersections on gridlines along the ray from the player instead of bullshit.
nYForm :: V2 Double -> V2 Double -> [V2 Double]
nYForm p r = ((p +) . (*^ nr)) <$> stepScales
    where
        nr = normalize r
        firstStep = deltaFirst (p^._y) (nr ^._y)
        stepScales = [(firstStep + y) / (abs (nr ^._y)) | y <- [0.0 .. 10.0]]

deltaFirst :: Double -> Double -> Double 
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px

--Lets test the ray regularly without concerning ourselves with the bumping into correct tile for wall positions. Maybe an epsilon aware rounder could work.
naivePath :: Int -> V2 Double -> V2 Double -> [(V2 Double, V2 Int)]
naivePath ws player direction = (epsilonBump direction) <$> mergeIntersections player vints hints
    where
        vints = clipWorld (fromIntegral ws) $ verticalIntersections player direction
        hints = clipWorld (fromIntegral ws) $ horizontalIntersections player direction

epsilon = 0.00001

epsilonBump :: V2 Double -> V2 Double -> (V2 Double, V2 Int)
epsilonBump ray result = (result,bumped)
    where
        bumped = V2 x y
        x = truncate $ result ^._x + (epsilon * signum ray ^._x) --Is this neccessary?
        y = truncate $ result ^._y + (epsilon * signum ray ^._y)

verticalIntersections = nXForm
horizontalIntersections = nYForm

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

p = V2 5.25 5.66
-- p = V2 0 0
r = V2 (1.0) (2.0)

-- foo = 
worldSize = 10

vints = clipWorld worldSize $ verticalIntersections p r
hints = clipWorld worldSize $ horizontalIntersections p r

--Test naivePath then naiveBumpPath
path = naivePath (fromIntegral worldSize) p r

wholeFloatE v = (v - fromIntegral (floor v)) < epsilon
v2OR :: V2 Bool -> Bool
v2OR (V2 x y) = x || y
path_test path = or $ fmap (v2OR . (fmap wholeFloatE) . fst) path --This should be true if all either member of the V2 Double of the path is on a gridline.

-- bumpPath = naiveBumpPath (fromIntegral worldSize) p r
visitedSet = S.fromList $ fmap snd path

--Test with grender
tgp = anonGP [
    worldGridTilesGraphic emptyMap visitedSet,
    worldGridGraphic worldSize,
    Prim $ Circle p 1 white,
    anonGP $ (\c -> Prim $ Circle c 1 blue) <$> vints,
    anonGP $ (\c -> Prim $ Circle c 1 red) <$> hints]

--TODO determine set of visited voxels