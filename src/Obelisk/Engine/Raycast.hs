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
        cameraPlaneSweep = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0]]--[0.. fromIntegral screenWidth]] TODO REVERT
        rayHeads = [position player + direction player + (camera_plane player ^* x) | x <- cameraPlaneSweep] :: [V2 Double]

type RayPath = [(DDAStep, Double)]
type RayResult = (RayPath, Double)

shootRay :: PVars -> V2 Double -> [(DDAStep, Double)]
shootRay player rayHeadOffset = rayPath rayAngle rayOrigin
    where
        rayAngle = vectorAngle (rayHeadOffset - position player)
        rayOrigin = convertToStep rayHeadOffset
        convertToStep (V2 x y) = Step x y

---------------------------------------------------------------------------
lenPassthrough :: [WallType] -> Int
lenPassthrough = length . takeWhile (/= FW)

-- visitedIndexes :: RayPath -> [V2 Int]
-- visitedIndexes = fmap (fmap floor) . intersectionPositions . fmap fst

wallSamples :: Vars -> [V2 Int] -> [WallType]
wallSamples gs [] = []
wallSamples gs (r:rs) = if inBounds gs r
                        then checkAt gs r : wallSamples gs rs
                        else []

--TODO test this
visitedPositions :: Vars -> RayPath -> S.Set (V2 Int)
visitedPositions gs steps = S.fromList $ take takeLength $ visitedIndexes steps
    where
        takeLength = lenPassthrough walls
        walls = wallSamples gs $ visitedIndexes steps

-------
trays :: [RayPath]
trays = genRays 1 initPVars
p1 :: RayPath
p1 = head trays
vi = visitedIndexes p1
vwalls = wallSamples initVars vi

vs :: S.Set (V2 Int)
vs = visitedPositions initVars p1

tx = take 5 . intersectionPositions . fmap fst $ p1
tpairs = zip tx $ tail tx
ty = take 5 . visitedIndexes $ p1

-- adjusedtedIndexes :: [V2 Double] -> [V2 Int]
-- adjusedtedIndexes (x:y:xs) = undefined 
-- adjusedtedIndexes [] = undefined 

--TODO we can fix the intersectionPositions by viewing the raypath stream as a ziping of the previous step to the current step
--We can check the current step to see which grid intersection is of interested for correct flooring/adjusting and peek at the previous
--TODO NOTE This probably should be a zipWith implementation

visitedIndexes :: RayPath -> [V2 Int]
visitedIndexes path = (floor <$> head xs) : zipWith wallCrossAdjust xs (tail xs)
    where 
        xs = intersectionPositions . fmap fst $ path

wholeThreshold :: Integer
wholeThreshold = 5

wallCrossAdjust :: V2 Double -> V2 Double -> V2 Int
wallCrossAdjust previous current = case fmap (isInt wholeThreshold) current of 
                                        V2 True True -> xyBump current
                                        V2 True _ -> xBump previous current
                                        _ -> yBump previous current

--If a ray is crossing away from floor bias, we need to check the previous intersection and the current
--if the current is on the x we check the previous x to see if its going against floor, same for y
xBump :: V2 Double -> V2 Double -> V2 Int
xBump (V2 px _) c@(V2 x y) = if px > x
                             then V2 (floor x - 1) (floor y)
                             else fmap floor c

yBump :: V2 Double -> V2 Double -> V2 Int
yBump (V2 _ py) c@(V2 x y) = if py > y
                             then V2 (floor x) (floor y - 1)
                             else fmap floor c

xyBump :: V2 Double -> V2 Int
xyBump (V2 x y) = V2 (floor x - 1) (floor y - 1)

tr :: [V2 Int]
tr = zipWith wallCrossAdjust tx $ tail tx

--TODO ROLL THIS INTO A REAL VISITED POSITIONS
tzzz = (floor <$> head tx) : tr
txxx = take 5 $ visitedIndexes p1
-----------
-- We need stuff for
--
-- Drawing the ray arrows
-- The positions they sample
-- The tiles they go through

-- RayPath -> (Set Positions, debugRay Primitives, SampledWallInfo)

type WallSampleInfo = (V2 Double, WallType)
castRay :: RayPath -> (S.Set (V2 Int), WallSampleInfo)
castRay = undefined

--https://stackoverflow.com/a/12868743
--Returns if x is an int to n decimal places
isInt :: (Integral a, RealFrac b) => a -> b -> Bool
isInt n x = round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0