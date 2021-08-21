{-# LANGUAGE GADTs #-}
module Obelisk.Graphics.Primitives where

import Obelisk.Math.Homogenous
--Consider how we should handle pixels

data Shape = Line
            | Circle
            | Triangle
            | Rectangle
            | Polygon
            | Curve

data Curve = Arc
           | Ellipse
           | Pie
           | Bezier

data ColorEffect = Lighten
                 | Darken

data Graphic a where
    Prim :: Shape -> Graphic Shape
    GroupPrim :: [Graphic Shape] -> Graphic Shape
    ColorPrim :: ColorEffect -> Graphic Shape -> Graphic Shape
    AffineT :: M22Affine Double -> Graphic Shape -> Graphic Shape

    --Traversable?

-- | Time signal. Goes from 0 to 1, inclusive.
type Time = Double
-- | Duration of an animation or effect. Usually measured in seconds.
type Duration = Double

data Animation = Animation Duration (Time -> Graphic Shape)

staticFrame :: Duration -> Graphic Shape -> Animation
staticFrame d g = Animation d (const g)
