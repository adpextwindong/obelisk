module Obelisk.State where

import Linear

--In the style of https://github.com/jxv/diner/library/DinoRo-rush/blob/mastush/State.hs
data PVars = PVars {
                position :: V2 Double,
                direction :: V2 Double
             }


data WallType = EW | FW | DW --Empty Wall, Full Wall, Door Wall
type WorldTiles = [[WallType]]
rFW = repeat FW
rEW = repeat EW

godboltMap :: [[WallType]]
godboltMap = [take 10 rFW,
              FW : take 3 rEW ++ [FW] ++ take 4 rEW ++ [FW],
              FW : take 3 rEW ++ [FW] ++ take 4 rEW ++ [FW],
              take 3 rFW ++ [DW] ++ [FW] ++ take 4 rEW ++ [FW],
              FW : take 3 rEW ++ [FW] ++ take 4 rEW ++ [FW],
              [FW,DW] ++ take 3 rFW ++ take 4 rEW ++ [FW],
              FW : take 3 rEW ++ take 4 rFW ++ [DW, FW],
              FW : take 8 rEW ++ [FW],
              FW : take 8 rEW ++ [FW],
              take 10 rFW]


data Vars = Vars {
                player :: PVars,
                map :: WorldTiles
            }