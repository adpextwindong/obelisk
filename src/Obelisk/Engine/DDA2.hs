module Obelisk.Engine.DDA2 where

import Linear
import Linear.V2
import Control.Lens
import Data.Functor
import qualified Data.Set as S

import Obelisk.Graphics.Primitives
import Obelisk.Graphics.DebugUI
import Obelisk.State (emptyMap)
import SDL.Primitive (horizontalLine)

--AmanatidesWoo Algorithmn
--We need to get actual intersection positions

--Positions in world space the raycaster will actually being checking
type DDAStep = V2 Double

--Start is included so the user checks they aren't in a wall
genRayPath :: V2 Double -> V2 Double -> [DDAStep]
genRayPath start@(V2 x y) v@(V2 vx  vy) = pullPosition <$> rayPath
    where
        (V2 stepX stepY) = computeStep v
        init_tMaxX = computeTmax x vx
        init_tMaxY = computeTmax y vy
        tDeltaX = 1.0 / vx
        tDeltaY = 1.0 / vy

        --TODO adapt this back to the original DDA algorithm in the Black Book
        rayPath = iterate rayPathIter (((x,y), (stepX, stepY)), (init_tMaxX, init_tMaxY))

        rayPathIter (((x,y), s@(stepX,stepY)), (tMaxX, tMaxY)) = if tMaxX < tMaxY
                                                            then (((x + stepX, y), s), (tMaxX + tDeltaX, tMaxY))
                                                            else (((x, y + stepY), s), (tMaxX, tMaxY + tDeltaY))

--Given the rayPathIter result it picks out the fst in the tripple and V2's it
pullPosition = uncurry V2 . fst . fst

--Computes how far on a number line we can travel while staying within the same "voxel" or whole number
computeTmax :: Double -> Double -> Double
computeTmax px vx = if vx < 0
                    then (px - (fromIntegral (floor px))) / vx
                    else (fromIntegral (ceiling px) - px) / vx

firstIntersection :: Double -> Double -> Double
firstIntersection px vx = if vx < 0
                          then fromIntegral $ floor px
                          else fromIntegral $ ceiling px

computeStep :: V2 Double -> V2 Double
computeStep v = fmap signToPlusNegOne v

signToPlusNegOne :: Double -> Double
signToPlusNegOne x = if x < 0
                     then -1
                     else 1

----------------------------------------------------------------

naivePath :: Int -> V2 Double -> V2 Double -> [V2 Double]
naivePath ws player direction = mergeIntersections player vints hints
    where
        vints = clipWorld (fromIntegral ws) $ verticalIntersections player direction
        hints = clipWorld (fromIntegral ws) $ horizontalIntersections player direction

-- All x's will end up at whole numbers on the rayline
-- TODO make a test to make sure these lay on the line
-- v ray
-- verticalIntersections :: V2 Double -> V2 Double -> [V2 Double]
-- verticalIntersections p@(V2 px _) v@(V2 vx _) = vert_rayPoints
--     where
--         eInds = evalInds px vx
--         vert_rayPoints = fmap ((p +). (v ^*)) eInds
verticalIntersections p r = xForm p r
horizontalIntersections p r = yForm p r

-- All y's will end up at whole numbers on the rayline
-- v is the ray
-- horizontalIntersections :: V2 Double -> V2 Double -> [V2 Double]
-- horizontalIntersections p@(V2 px py) v@(V2 _ vy) = horz_rayPoints --TODO
--     where
--         eInds = evalInds py vy
--         horz_rayPoints = fmap ((p +). ((swapScale v) ^*)) eInds

--SHIT
-- firstIntersections :: V2 Double -> V2 Double -> ([V2 Double],[V2 Double])
-- firstIntersections p@(V2 px py) r@(V2 rx ry) = (xIntersections yInterBase r 0, yIntersections xInterBase r 0)
--     where 
--         (xFirst, yFirst) = deltaFirsts p r
--         yInterBase = V2 (xFirst + px) (py + (xFirst*ry))
        
--         xInterBase = V2 (px + (yFirst * rx)) (yFirst + py)

-- xIntersections yB r df = [yB + xStep r i  | i <- [df, df+1.0..]]
-- yIntersections xB r df = [xB + yStep r i | i <- [df, df+1.0..]]
-- deltaFirsts p r = (deltaFirst (p^._x) (r^._x), deltaFirst (p^._y) (r^._y))
-- yStep r t = t *^ r ^* (1/ abs (r^._y))
-- xStep r t = t *^ r ^* (1/ abs (r^._x))

--TODO test these forms with rays in all directions

{-
$\hat{p} - \Delta\hat{v} = < I , \_>$
Find the integer solutions of this form

px + delta*vx = Ix
delta * vx = Ix - Px
delta = (Ix - Px) / Vx

We compute delta for every i'th intersection on the grid we're looking for and scale r and add it to the player's original position
-}
xForm :: V2 Double -> V2 Double -> [V2 Double]
xForm p r = [((i - p^._x)/(r^._x)) *^ r + p | i <- fmap (signToPlusNegOne (r^._x) *) [1.00..10.0]]
yForm :: V2 Double -> V2 Double -> [V2 Double]
yForm p r = [((i - p^._y)/(r^._y)) *^ r + p | i <- fmap (signToPlusNegOne (r^._y) *) [1.00..10.0]]


--TODO it might be simpler to utilize Linear.Affine
--We could write a property test that says these points all trivially appear to be on the ray when affine translated by the player point?
--At this point lets start using QuickCheck or something to start testing this stuff

-- verticalPoints :: V2 Double -> V2 Double -> [V2 Double]

deltaFirst :: Double -> Double -> Double 
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px
--TODO make sure this works with rays in all directions
absDeltaFirst :: Double -> Double -> Double
absDeltaFirst px vx = if vx < 0
                      then px - (fromIntegral (floor px))
                      else (fromIntegral (ceiling px)) - px

--Eval the ray at these points and add player position to get the intersection points
evalInds px vx = (\b -> [b, (b + 1.0) ..]) (deltaFirst px vx)


naiveBumpPath :: Int -> V2 Double -> V2 Double -> [(V2 Double, V2 Int)]
naiveBumpPath ws player direction = mergeBumpIntersections player direction vints hints
    where
        vints = clipWorld (fromIntegral ws) $ verticalIntersections player direction
        hints = clipWorld (fromIntegral ws) $ horizontalIntersections player direction

mergeIntersections :: V2 Double -> [V2 Double] -> [V2 Double] -> [V2 Double]
mergeIntersections player (x:xs) (y:ys) = if distance player x < distance player y
                                          then x : mergeIntersections player xs (y:ys)
                                          else y : mergeIntersections player (x:xs) ys
mergeIntersections _ [] ys = ys
mergeIntersections _ xs [] = xs

mergeBumpIntersections :: V2 Double -> V2 Double -> [V2 Double] -> [V2 Double] -> [(V2 Double, V2 Int)]
mergeBumpIntersections player ray (x:xs) (y:ys) = if distance player x < distance player y
                                                  then (x, xBumpFloorRay ray x) : mergeBumpIntersections player ray xs (y:ys)
                                                  else (y, yBumpFloorRay ray y) : mergeBumpIntersections player ray (x:xs) ys
mergeBumpIntersections _ ray [] ys = (\y -> (y, yBumpFloorRay ray y)) <$> ys
mergeBumpIntersections _ ray xs [] = (\x -> (x, xBumpFloorRay ray x)) <$> xs
--is xybump needed?

xBumpFloorRay :: V2 Double -> V2 Double -> V2 Int
xBumpFloorRay (V2 vx _) inter@(V2 x y) = if vx < 0
                                         then fmap round inter--V2 ((floor x) - 1) (floor y)
                                         else V2 (round x) (truncate y)

yBumpFloorRay :: V2 Double -> V2 Double -> V2 Int
yBumpFloorRay (V2 _ vy) inter@(V2 x y) = if vy < 0
                                         then fmap round inter --V2 (floor x) ((floor y) - 1)
                                         else V2 (truncate x) (round y)  --TODO make sure this is right
--TODO check if this produces the correct traversal

swapScale :: Fractional a => V2 a -> V2 a
swapScale v@(V2 _ y) = (1.0/y) *^ v
-- tgp =  GroupPrim "" $ (\c -> Prim (Circle c 1 (V4 255 255 255 255)))
            -- <$> [V2 x y |  x <- [0..10], y <- [0..10]]

clipWorld ws = takeWhile (\v@(V2 x y) -> x <= fromIntegral ws && y <= fromIntegral ws
                                            && x >= 0 && y >= 0)

p = V2 0.25 0.66
-- p = V2 0 0
r = V2 1 2

-- foo = 
worldSize = 10

vints = clipWorld worldSize $ verticalIntersections p r
hints = clipWorld worldSize $ horizontalIntersections p r

path = naivePath (fromIntegral worldSize) p r
bumpPath = naiveBumpPath (fromIntegral worldSize) p r
visitedSet = S.fromList $ fmap snd bumpPath

tgp = anonGP [
    worldGridTilesGraphic emptyMap visitedSet,
    worldGridGraphic worldSize,
    Prim $ Circle p 1 white,
    anonGP $ (\c -> Prim $ Circle c 1 blue) <$> vints,
    anonGP $ (\c -> Prim $ Circle c 1 red) <$> hints]

--TODO determine set of visited voxels