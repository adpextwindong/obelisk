module Raycaster where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array
import Data.List
import System.Random

-- Types we need
-- Bitmap type
-- Player record
-- Map type
-- Camera record for details on focal length and stuff

data Player = Player {
                       pX :: Float
                     , pY :: Float
                     , direction :: Float
                     , paces :: Int
                     }

pRotate :: Player -> Player
pRotate _ = undefined --TODO

pWalk :: Player -> Player
pWalk _ = undefined --TODO

data Controls = Controls

pUpdate :: Player -> Controls -> Map -> Float -> Player
pUpdate player controls map elapsedFrameTime = undefined --TODO

--We need some sort of update function for controls using elapsed time, map and control state for the frame

data Map = Map {
                 msize :: Int -- we could drop this and use the array bounds fn
               , wallGrid :: Array (Int,Int) Float
               -- , light :: Int
               }

getMapLoc :: Map -> (Int,Int) -> Maybe Float
getMapLoc m ind@(x,y) = if inBounds m ind
                        then Just $ wallGrid m ! ind
                        else Nothing
    where
    inBounds m (x,y) = x > -1 && x < s &&
                       y > -1 && y < s
        where
            s = msize m

generateMap seed size = Map size $ listArray ((0,0) , (size - 1, size - 1)) $ take (size * size) $ rolls pureGen
    where
        pureGen = mkStdGen seed
        roll = uniformR (0.0 :: Float, 1.0 :: Float)
        rolls = unfoldr (Just . roll)


data Camera = Camera {
                       width :: Int
                     , height :: Int
                     , resolution :: Int -- ??
                     , focalLength :: Float
                     , range :: Int
                     , lightRange :: Int
                     , scale :: Int
                     }
-- TODO find a bitmap type the SDL one can probably work

cameraResolution = 320
drawDistLimitRange = 10000.0 --This is range in the original code

drawColumns :: (MonadIO m) => Player -> Map -> Camera -> m ()
drawColumns player map camera = forM_ columns
            (\colIndex -> do
                              let screenX = (fromIntegral colIndex / fromIntegral cameraResolution) - 0.5 :: Float
                              let rayAngle = atan2 screenX cameraFocalLength
                              --TODO RENDER COL
                              let raySteps = rayCast (pX player, pY player) (direction player + rayAngle)
                              drawColumn colIndex raySteps rayAngle map)
    where
        cameraResolution = resolution camera
        cameraFocalLength = focalLength camera
        columns = [0..cameraResolution - 1]


data RayStepSample = RayStepSample {
                    rayHeight :: Float,
                    rayDistance :: Float,
                    rayOffset :: Float
               }


rayCast :: (Float, Float) -> Float -> [RayStepSample]
rayCast originPoint angle = undefined --TODO pull from previous code

drawColumn :: (MonadIO m) => Int -> [RayStepSample] -> Float -> Map -> m ()
drawColumn = undefined --TODO

projectWallHeight :: Float -> Float -> Float -> Float -> (Float, Float)
projectWallHeight canvasHeight sampledHeight angle distance = (bottom - wallHeight, wallHeight)
    where
        z = distance * cos angle
        wallHeight = canvasHeight * sampledHeight / z
        bottom = canvasHeight / 2 * (1 + 1 / z)
