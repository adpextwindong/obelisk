{-# LANGUAGE GADTs #-}
module Obelisk.Graphics.Primitives where

import qualified SDL
import qualified SDL.Primitive as SDL

import Obelisk.Math.Homogenous
--Consider how we should handle pixels

data Shape = Line SDL.Pos SDL.Pos SDL.Color
            | Circle SDL.Pos SDL.Radius SDL.Color
            | FillTriangle SDL.Pos SDL.Pos SDL.Pos SDL.Color
            | FillCircle SDL.Pos SDL.Radius SDL.Color
            deriving Show

applyAffineTransform :: M22Affine Double -> Shape -> Shape
applyAffineTransform t (Line start end color) = Line (f start) (f end) color
    where f = dropHomoCoords . doubleTransformFloor t . homoCoords

--             | Triangle
--             | Rectangle
--             | Polygon
--             | Curve

-- data Curve = Arc
--            | Ellipse
--            | Pie
--            | Bezier

-- data ColorEffect = Lighten
--                  | Darken

-- Used to phase graphics that we've applied all transformations and effects
-- so that we only draw it in the renderer once its all applied
-- Phasing it this way can help us look at the scene's graphics before and after transformations are applied I guess
data Evaluated a = Evaluated a
    deriving Show
    
data Graphic a where
    Prim :: Shape -> Graphic Shape
    GroupPrim :: [Graphic Shape] -> Graphic Shape
    -- ColorPrim :: ColorEffect -> Graphic Shape -> Graphic Shape
    AffineT :: M22Affine Double -> Graphic Shape -> Graphic Shape
    EvaldP :: Graphic Shape -> Graphic (Evaluated Shape)
    EvaldGP :: [Graphic (Evaluated Shape)] -> Graphic (Evaluated Shape)
    --At evaluation we floor at the end

instance (Show a) => Show (Graphic a) where
    show (Prim s) = "Prim " ++ show s
    show (GroupPrim xs) = "GroupPrim " ++ show xs
    show (AffineT t s) = "AffineT " ++ show t ++ show s
    show (EvaldP s) = "Evaluated Prim" ++ show s
    show (EvaldGP gs) = "Evaluated GroupPrim" ++ show gs

--TODO COLOR, EFFECTS, ANIMATIONS, SCENES LATER

-- | Time signal. Goes from 0 to 1, inclusive.
type Time = Double
-- | Duration of an animation or effect. Usually measured in seconds.
type Duration = Double

data Animation = Animation Duration (Time -> Graphic Shape)

staticFrame :: Duration -> Graphic Shape -> Animation
staticFrame d g = Animation d (const g)
