module Obelisk.Math.Vector where

import Linear

vectorAngle :: V2 Float -> Float
vectorAngle (V2 x y)
    | y > 0 = atan2 y x
    | otherwise = 2 * pi + atan2 y x

cosThetaBetween :: V2 Float -> V2 Float -> Float
cosThetaBetween v u = dot u v / (norm u * norm v)