module Obelisk.Engine.Raycast where

import Foreign.C.Types
import Linear
import Control.Lens
import qualified Data.Set as S

import Obelisk.State
import Obelisk.Engine.DDA
import Obelisk.Math.Vector

genRays :: CInt -> PVars -> [[(DDAStep,Double)]]
genRays screenWidth player = fmap (shootRay player) rayHeads
    where
        cameraPlaneSweep = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0.. fromIntegral screenWidth]]
        rayHeads = [position player + direction player + (camera_plane player ^* x) | x <- cameraPlaneSweep] :: [V2 Double]

shootRay :: PVars -> V2 Double -> [(DDAStep, Double)]
shootRay player rayHeadOffset = rayPath rayAngle rayOrigin
    where
        rayAngle = vectorAngle (rayHeadOffset - position player)
        rayOrigin = convertToStep rayHeadOffset
        convertToStep (V2 x y) = Step x y

---------------------------------------------------------------------------
lenPassthrough :: [WallType] -> Int
lenPassthrough = length . takeWhile (/= FW)

--TODO THIS NEEDS TO BE AN ARRAY NOW!!
checkAt :: Vars -> V2 Int -> WallType
checkAt gs (V2 x y) = accessMap (world gs) x y
    
visitedIndexes :: RayPath -> [V2 Int]
visitedIndexes = fmap (fmap floor) . intersectionPositions . fmap fst 

wallSamples :: Vars -> [V2 Int] -> [WallType]
wallSamples gs [] = []
wallSamples gs (r:rs) = if inBounds gs r
                        then checkAt gs r : wallSamples gs rs
                        else []

inBounds :: Vars -> V2 Int -> Bool
inBounds gs (V2 x y) = x >= 0 && y >= 0 && x < limit && y < limit
    where limit = fromIntegral . worldSize . world $ gs

type RayPath = [(DDAStep, Double)]

visitedPositions :: Vars -> RayPath -> S.Set (V2 Int)
visitedPositions gs steps = S.fromList $ take takeLength $ visitedIndexes steps
    where
        takeLength = lenPassthrough walls + 1
        walls = wallSamples gs $ visitedIndexes steps

-------
trays = genRays 1 initPVars 