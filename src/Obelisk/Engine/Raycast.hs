module Obelisk.Engine.Raycast where

import Foreign.C.Types
import Linear
import Control.Lens

import Obelisk.State
import Obelisk.Engine.DDA
import Obelisk.Math.Vector

genRays :: CInt -> PVars -> [[(DDAStep,Double)]]
genRays screenWidth player = fmap (\rH -> rayPath (rayAngle rH) (convertToStep rH)) rayHeads
    where
        cameraPlaneSweep = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0.. fromIntegral screenWidth]]
        rayHeads = [position player + direction player + (camera_plane player ^* x) | x <- cameraPlaneSweep] :: [V2 Double]
        rayAngle rH = vectorAngle (rH - position player)
        convertToStep (V2 x y) = Step x y

---------------------------------------------------------------------------
tgr :: [[(DDAStep, Double)]]
tgr = genRays 4 (player initVars)

--TODO turn off rotation, paint visited tiles for a single ray
tpr :: PVars -> [V2 Double]
tpr player = intersectionPositions $ fmap fst intersections
    where
        pAngle = vectorAngle $ direction player
        pOrigin = Step (position player ^._x) (position player ^._y)
        intersections = take 10 $ rayPath pAngle pOrigin :: [(DDAStep, Double)]

tpx = tpr $ player initVars 