module Obelisk.Engine.DDA2 where

import Linear

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

firstIntersection px vx = if vx < 0
                          then fromIntegral $ floor px
                          else fromIntegral $ ceiling px

computeStep :: V2 Double -> V2 Double
computeStep v = fmap signToPlusNegOne v

signToPlusNegOne :: Double -> Double
signToPlusNegOne x = if x < 0
                     then -1
                     else 1