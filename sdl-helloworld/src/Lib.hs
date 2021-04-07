module Lib
    ( someFunc
    ) where

import Data.Array
import Data.List
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
pRotate _ = undefined

pWalk :: Player -> Player
pWalk _ = undefined

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



main :: IO ()
main = do
    return ()
