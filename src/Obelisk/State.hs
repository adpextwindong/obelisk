{-# LANGUAGE TemplateHaskell #-}
module Obelisk.State where

import qualified SDL.Video.Renderer as SDL
import Obelisk.Math.Homogenous
import Obelisk.Engine.Input
import Obelisk.Types.Wall
import Control.Lens
import Prelude hiding (map)
import Linear
import Foreign.C.Types
import Data.Array.Unboxed

--In the style of https://github.com/jxv/diner/library/DinoRo-rush/blob/mastush/State.hs
--TODO make gamevars strict
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

data Vars = Vars {
                player :: PVars,
                world :: WorldTiles,
                --Debug vars TODO refactor
                rotateToPView :: Bool,
                vInput :: Input,
                worldGTP :: V3 (V3 Float),
                camZoomScale :: Float, --Used for scaling other things
                viewMode :: ViewMode,
                config :: GConfig
            }
    deriving Show

data GConfig = GConfig {
  projectionType :: ProjectionType
}
  deriving Show

data ProjectionType = Permadi | FishEye
  deriving Show

data ViewMode = PlayerPOV | OverheadDebug
  deriving Show

{-# INLINE accessMapV #-}
accessMapV :: WorldTiles -> V2 Int -> WallType
accessMapV w (V2 x y) = accessMap w x y

{-# INLINE accessMap #-}
accessMap :: WorldTiles -> Int -> Int -> WallType
accessMap world x y = mapTiles world ! ((x * fromIntegral (worldSize world)) + y)

accessIndex world x y = ((x * fromIntegral (worldSize world)) + y)

checkAt :: Vars -> V2 Int -> WallType
checkAt gs (V2 x y) = accessMap (world gs) x y

inBounds :: Vars -> V2 Int -> Bool
inBounds gs (V2 x y) = x >= 0 && y >= 0 && x < limit && y < limit
    where limit = fromIntegral . worldSize . world $ gs

rFW :: [WallType]
rFW = repeat $ FW 0 SDL.BlendNone
rEW :: [WallType]
rEW = repeat EW

--ACCESSED godBoltMap !! y !! x style
{-
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
-}

boxMap :: WorldTiles
boxMap = WorldTiles map 10
    where bt = FW 0 SDL.BlendNone
          map = listArray (0, 99) $ concat [take 10 rFW,
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 bt : take 8 rEW ++ [bt],
                 take 10 rFW
                ]

boxMap64 :: WorldTiles
boxMap64 = WorldTiles map 64
    where map = listArray (0, 4095) $ take 64 rFW ++ concat (replicate 62 (FW 0 SDL.BlendNone : take 62 rEW ++ [FW 0 SDL.BlendNone])) ++ take 64 rFW

emptyMap = WorldTiles (listArray (0, 99) rEW) 10

initPVars :: PVars
initPVars = PVars (V2 2.5 6.5) dir cam
    where
        dir = normalize (V2 0.8 0.330)
        cam = normalize $ dir *! rotation2 (pi/2)

initVars = Vars initPVars boxMap False initInput baseGTP baseZoomScale OverheadDebug defaultConfig
  where
    baseGTP = rawCenterScreenOnWorldGrid 10 640 480
    baseZoomScale = 1.0

--TODO add input config toggle for projection type
defaultConfig = GConfig Permadi

makeClassy ''Vars
makeClassy ''PVars
makeClassy ''WorldTiles

instance HasPVars Vars where
    pVars = lens player (\v s -> v { player = s})

instance HasWorldTiles Vars where
    worldTiles = lens world (\v s -> v { world = s})
