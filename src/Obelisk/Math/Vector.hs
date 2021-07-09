module Obelisk.Math.Vector where

import Linear

vectorAngle :: V2 Double -> Double
vectorAngle (V2 x y)
    | y > 0 = atan2 y x
    | otherwise = 2 * pi + atan2 y x


