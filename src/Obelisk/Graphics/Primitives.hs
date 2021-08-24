{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Obelisk.Graphics.Primitives where

import qualified SDL
import qualified SDL.Primitive as SDL
import Linear.V2 ( V2 )
import Foreign.C.Types (CInt)

import Obelisk.Math.Homogenous
--Consider how we should handle pixels

--TODO for CONSIDERATION Should these be SDL.Pos or V2 a's ???
data Shape a = Line (V2 a) (V2 a) SDL.Color
             | Circle (V2 a) SDL.Radius SDL.Color
             | FillTriangle (V2 a) (V2 a) (V2 a) SDL.Color
             | FillCircle (V2 a) SDL.Radius SDL.Color
            deriving Show

applyAffineTransformFloor :: M22Affine Double -> Shape Double -> Shape CInt
applyAffineTransformFloor t (Line start end color)           = Line (mapAft t start) (mapAft t end) color
applyAffineTransformFloor t (Circle center radius color)     = Circle (mapAft t center) radius color
applyAffineTransformFloor t (FillTriangle v0 v1 v2 color)    = FillTriangle (mapAft t v0) (mapAft t v1) (mapAft t v2) color
applyAffineTransformFloor t (FillCircle center radius color) = FillCircle (mapAft t center) radius color

-- Takes a regular vector, wraps into a homogeonous coordinate system for applying an affine transformation, floors it to an integer to be used in a draw call render
mapAft :: M22Affine Double -> V2 Double -> V2 CInt
mapAft t = dropHomoCoords . transformFloor t . homoCoords 

--TODO finish other patterns

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
-- data Evaluated a b = Evaluated a b
--     deriving Show
    
data Graphic a where
    Prim :: Shape Double -> Graphic (Shape Double)
    GroupPrim :: [Graphic (Shape Double)] -> Graphic (Shape Double)
    -- ColorPrim :: ColorEffect -> Graphic Shape -> Graphic Shape
    AffineT :: M22Affine Double -> Graphic (Shape Double) -> Graphic (Shape Double)
    EvaldP :: Shape CInt -> Graphic (Shape CInt)
    EvaldGP :: [Graphic (Shape CInt)] -> Graphic (Shape CInt)
    --At evaluation we floor at the end

instance Show (Graphic (Shape Double)) where
    show (Prim s) = "Prim " ++ show s
    show (GroupPrim xs) = "GroupPrim " ++ show xs
    show (AffineT t s) = "AffineT " ++ show t ++ show s

instance Show (Graphic (Shape CInt)) where
    show (EvaldP s) = "Evaluated Prim" ++ show s
    show (EvaldGP gs) = "Evaluated GroupPrim" ++ show gs

--TODO COLOR, EFFECTS, ANIMATIONS, SCENES LATER

-- | Time signal. Goes from 0 to 1, inclusive.
type Time = Double
-- | Duration of an animation or effect. Usually measured in seconds.
type Duration = Double

-- data Animation = Animation Duration (Time -> Graphic Shape)

-- staticFrame :: Duration -> Graphic Shape -> Animation
-- staticFrame d g = Animation d (const g)
