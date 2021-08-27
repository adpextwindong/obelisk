module Obelisk.Engine.DDA2 where

import Linear
import Linear.V2
import Control.Lens
import Data.Functor

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

naivePath :: V2 Double -> V2 Double -> [V2 Double]
naivePath player direction = undefined

-- All x's will end up at whole numbers on the rayline
-- TODO make a test to make sure these lay on the line
-- TODO reuse the code that handles correct flooring/rounding for conversion to tile indexes
verticalIntersections :: V2 Double -> V2 Double -> [V2 Double]
verticalIntersections p@(V2 px _) v@(V2 vx _) = vert_rayPoints
    where
        eInds = evalInds px vx
        vert_rayPoints = fmap ((p +). (v ^*)) eInds

-- All y's will end up at whole numbers on the rayline
horizontalIntersections :: V2 Double -> V2 Double -> [V2 Double]
horizontalIntersections p@(V2 _ py) v@(V2 _ vy) = undefined --TODO

absDeltaFirst :: Double -> Double -> Double
absDeltaFirst px vx = if vx < 0
                   then px - (fromIntegral (floor px))
                   else (fromIntegral (ceiling px)) - px

--Eval the ray at these points and add player position to get the intersection points
evalInds px vx = (\absf -> [absf, (absf + 1.0) ..]) (absDeltaFirst px vx)

mergeIntersections :: V2 Double -> [V2 Double] -> [V2 Double] -> [V2 Double]
mergeIntersections player (x:xs) (y:ys) = if distance player x < distance player y
                                          then x : mergeIntersections player xs (y:ys)
                                          else y : mergeIntersections player (x:xs) ys