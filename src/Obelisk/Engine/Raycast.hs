module Obelisk.Engine.Raycast where

import Foreign.C.Types
import Linear
import Control.Lens
import qualified Data.Set as S
import Data.Fixed (mod')
import Data.Bifunctor

import Obelisk.State
import Obelisk.Engine.DDA
import Obelisk.Math.Vector

type RayAngle = Double
type RayPath = [(DDAStep, Double)]

genRays :: CInt -> PVars -> [(RayPath, RayAngle)]
genRays screenWidth player = fmap (first (shootRay player)) rays
    where
        cameraPlaneSweep = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0.. fromIntegral screenWidth]]
        rayHead x = position player + direction player + (camera_plane player ^* x)
        rayAngle x = vectorAngle $ direction player + (camera_plane player ^* x) --TODO smell
        rays = [(rayHead x, rayAngle x) | x <- cameraPlaneSweep] :: [(V2 Double, RayAngle)]

shootRay :: PVars -> V2 Double -> RayPath
shootRay player rayHeadOffset = rayPath rayAngle rayOrigin
    where
        rayAngle = vectorAngle (rayHeadOffset - position player) --TODO smell
        rayOrigin = convertToStep rayHeadOffset
        convertToStep (V2 x y) = Step x y

---------------------------------------------------------------------------
lenPassthrough :: [WallType] -> Int
lenPassthrough = length . takeWhile (/= FW)

visitedIndexes :: RayPath -> [V2 Int]
visitedIndexes path = fmap (fmap floor) . intersectionPositions $ steps
    where steps = fmap fst path :: [DDAStep]

visitedProperIndexes :: RayPath -> RayAngle -> [V2 Int]
visitedProperIndexes path angle = fmap (quadrantFlooring quad) . intersectionPositions $ steps
    where steps = fmap fst path
          quad = quadrant angle

visitedProperPositions :: Vars -> RayPath -> RayAngle -> S.Set (V2 Int)
visitedProperPositions gs steps angle = S.fromList $ take takeLength visitSet
    where takeLength = lenPassthrough walls + 1
          visitSet = visitedProperIndexes steps angle
          walls = wallSamples gs visitSet

trs = genRays 1 (player initVars)
vrs = S.unions $ uncurry (visitedProperPositions initVars) <$> [head trs]

data Quadrant = QI | QII | QIII | QIV
    deriving Show

quadrant :: RayAngle -> Quadrant
quadrant angle
    | angle >= 0.0        && angle < pi / 2     = QI
    | angle >= pi / 2     && angle < pi         = QII
    | angle >= pi         && angle < 3 * pi / 2 = QIII
    | angle >= 3 * pi / 2 && angle < 2 * pi     = QIV
    | otherwise = quadrant $ mod' angle (2 * pi)

--A bifunctor instance on V2 would be cool...
quadrantFlooring :: Quadrant -> V2 Double -> V2 Int 
quadrantFlooring QI pos = fmap floor pos
quadrantFlooring QII (V2 x y) = V2 (floor x) (floor y)
quadrantFlooring QIII pos = fmap floor pos
quadrantFlooring QIV  (V2 x y) = V2 (floor x) (floor y)

tpos = V2 1.0 1.5
tpos2 = V2 0.8 1.0
tpos3 = V2 1.5 1.0
tpos4 = V2 1.0 0.8

floorSub v = floor v - 1

wallSamples :: Vars -> [V2 Int] -> [WallType]
wallSamples gs [] = []
wallSamples gs (r:rs) = if inBounds gs r
                        then checkAt gs r : wallSamples gs rs
                        else []


visitedPositions :: Vars -> RayPath -> S.Set (V2 Int)
visitedPositions gs steps = S.fromList $ take takeLength $ visitedIndexes steps
    where
        takeLength = lenPassthrough walls
        walls = wallSamples gs $ visitedIndexes steps

-------
trays = genRays 1 initPVars


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
