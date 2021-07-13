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
tgr :: [[(DDAStep, Double)]]
tgr = genRays 4 (player initVars)

--TODO turn off rotation, paint visited tiles for a single ray
tpr :: PVars -> [V2 Double]
tpr player = intersectionPositions $ fmap fst intersections
    where
        pAngle = vectorAngle $ direction player
        pOrigin = Step (position player ^._x) (position player ^._y)
        intersections = take 10 $ drop 1 $ rayPath pAngle pOrigin :: [(DDAStep, Double)]

tpx = tpr $ player initVars 
ftpx = fmap (fmap floor) tpx
cftpx = checkRay initVars
lenPassthrough = length . takeWhile (/= FW) $ cftpx


checkRay :: Vars -> [WallType]
checkRay gs = fmap checkV2 ftpx
    where
        checkV2 (V2 x y) = check x y
        check x y = mapTiles (world gs) !! y !! x

--TODO test this by spinning the player around
visitedSet :: S.Set (V2 CInt)
visitedSet = S.fromList (fmap fromIntegral <$> take (lenPassthrough + 1) ftpx)

--TODO make a monadic version that gets the users's current field of view

type RayPath = [(DDAStep, Double)]

visitedPositions :: [RayPath] -> S.Set (V2 CInt)
visitedPositions rays = undefined