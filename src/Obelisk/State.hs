{-# LANGUAGE TemplateHaskell #-}
module Obelisk.State where

import Obelisk.Math.Homogenous
import Obelisk.Engine.Input
import Obelisk.Types.Wall
import Control.Lens
import Prelude hiding (map)
import Linear
import Foreign.C.Types
import Data.Array.Unboxed

--In the style of https://github.com/jxv/diner/library/DinoRo-rush/blob/mastush/State.hs
data PVars = PVars {
                position :: V2 Float,
                direction :: V2 Float,
                camera_plane :: V2 Float
             } deriving (Show)


data WorldTiles = WorldTiles {
                    mapTiles :: Array Int WallType,
                    worldSize :: CInt
                  }
    deriving Show

{-# INLINE accessMapV #-}
accessMapV :: WorldTiles -> V2 Int -> WallType
accessMapV w (V2 x y) = accessMap w x y

{-# INLINE accessMap #-}
accessMap :: WorldTiles -> Int -> Int -> WallType
accessMap world x y = mapTiles world ! ((x * fromIntegral (worldSize world)) + y)

checkAt :: Vars -> V2 Int -> WallType
checkAt gs (V2 x y) = accessMap (world gs) x y

inBounds :: Vars -> V2 Int -> Bool
inBounds gs (V2 x y) = x >= 0 && y >= 0 && x < limit && y < limit
    where limit = fromIntegral . worldSize . world $ gs

rFW :: [WallType]
rFW = repeat FW
rEW :: [WallType]
rEW = repeat EW

--ACCESSED godBoltMap !! y !! x style
godboltMap :: WorldTiles
godboltMap = WorldTiles map 10
    where map = listArray (0, 10*10 - 1) $ concat [take 10 rFW,
              FW : take 3 rEW ++ [FW] ++ take 4 rEW ++ [FW],
              FW : take 3 rEW ++ [FW] ++ take 4 rEW ++ [FW],
              take 3 rFW ++ [DW] ++ [FW] ++ take 4 rEW ++ [FW],
              FW : take 3 rEW ++ [FW] ++ take 4 rEW ++ [FW],
              [FW,DW] ++ take 3 rFW ++ take 4 rEW ++ [FW],
              FW : take 3 rEW ++ take 4 rFW ++ [DW, FW],
              FW : take 8 rEW ++ [FW],
              FW : take 8 rEW ++ [FW],
              take 10 rFW]

boxMap :: WorldTiles
boxMap = WorldTiles map 10
    where map = listArray (0, 99) $ concat [take 10 rFW,
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 FW : take 8 rEW ++ [FW],
                 take 10 rFW
                ]

boxMap64 :: WorldTiles
boxMap64 = WorldTiles map 64
    where map = listArray (0, 4095) $ take 64 rFW ++ concat (replicate 62 (FW : take 62 rEW ++ [FW])) ++ take 64 rFW

emptyMap = WorldTiles (listArray (0, 99) rEW) 10

data Vars = Vars {
                player :: PVars,
                world :: WorldTiles,
                --Debug vars TODO refactor
                rotateToPView :: Bool,
                vInput :: Input
            }
    deriving Show

initPVars :: PVars
initPVars = PVars (V2 2.5 6.5) dir cam
    where
        -- dir = normalize (V2 0.8 0.330)
        -- cam = normalize $ dir *! rotation2 (-pi/2)

        -- dir = V2 0.8817506897247581 0.4717157207152668
        -- cam = V2 (-0.4717157207152668) 0.8817506897247581

        -- dir = V2 0.17859740053185252 0.9839222370305881
        -- cam = V2 (-0.9839222370305881) 0.17859740053185252

        --Boxmap
        -- dir = V2 0.9304581537328835 (-0.36639817705888417)
        -- cam = V2 0.36639817705888417 0.9304581537328835
         dir = V2 9.551545757406914e-2 (-0.9954279468472328)
         cam = V2 0.9954279468472328 9.551545757406914e-2

initVars :: Vars
initVars = Vars initPVars boxMap False initInput

makeClassy ''Vars
makeClassy ''PVars
makeClassy ''WorldTiles

instance HasPVars Vars where
    pVars = lens player (\v s -> v { player = s})

instance HasWorldTiles Vars where
    worldTiles = lens world (\v s -> v { world = s})