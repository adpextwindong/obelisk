{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Graphics.Primitives where

import qualified SDL
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Primitive as SDL
import Linear
import Foreign.C.Types (CInt)

import Data.Coerce
import Obelisk.Math.Homogenous
import Obelisk.Types.Wall

--Consider how we should handle pixels
data Shape a = Line (V2 a) (V2 a) SDL.Color
             | Circle (V2 a) SDL.Radius SDL.Color
             | FillTriangle (V2 a) (V2 a) (V2 a) SDL.Color
             | FillCircle (V2 a) SDL.Radius SDL.Color
             | FillRectangle (V2 a) (V2 a) SDL.Color
             | CopyRect SDL.Texture (V2 CInt) (V2 CInt) (V2 a) (V2 a) SDL.BlendMode --Textured Rect

instance (Show a) => Show (Shape a) where
  show (Line start end c) = "Line " ++ show start ++ " " ++ show end ++ " " ++ show c
  show (Circle center r c) = "Circle " ++ show center ++ " " ++ show r ++ " " ++ show c
  show (FillTriangle a b c color) = "FillTriangle " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show color
  show (FillCircle center r c) = "FillCircle " ++ show center ++ " " ++ show r ++ " " ++ show c
  show (FillRectangle a b color) = "FillRectangle " ++ show a ++ " " ++ show b ++ " " ++ show color
  show (CopyRect _ srcStart srcSize dstStart dstEnd transparency) = "CopyRect " ++ show srcStart ++ " " ++ show srcSize ++ " " ++ show dstStart ++ " "  ++ show dstEnd ++ " " ++ show transparency

applyAffineTransformFloor :: M22Affine Float -> Shape Float -> Shape CInt
applyAffineTransformFloor t (Line start end color)           = Line (mapAft t start) (mapAft t end) color
applyAffineTransformFloor t (Circle center radius color)     = Circle (mapAft t center) radius color
applyAffineTransformFloor t (FillTriangle v0 v1 v2 color)    = FillTriangle (mapAft t v0) (mapAft t v1) (mapAft t v2) color
applyAffineTransformFloor t (FillRectangle v0 v1 color)      = FillRectangle (mapAft t v0) (mapAft t v1) color
applyAffineTransformFloor t (FillCircle center radius color) = FillCircle (mapAft t center) radius color
applyAffineTransformFloor t (CopyRect txt src size dstTopLeft dstBottomRight transparency)      = CopyRect txt src size (mapAft t dstTopLeft) (mapAft t dstBottomRight) transparency

-- Takes a regular vector, wraps into a homogeonous coordinate system for applying an affine transformation, floors it to an integer to be used in a draw call render
mapAft :: M22Affine Float -> V2 Float -> V2 CInt
mapAft t = dropHomoCoords . transformFloor t . homoCoords

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
    Prim :: Shape Float -> Graphic Float
    GroupPrim :: String -> [Graphic Float] -> Graphic Float
    -- ColorPrim :: ColorEffect -> Graphic Shape -> Graphic Shape
    --Show t constraint could be removed, we're just using this coerce stuff for Data.Tagged phantom typing
    AffineT :: (Coercible t (M22Affine Float), Show t) => t -> Graphic a -> Graphic a
    EvaldP :: Shape CInt -> Graphic CInt
    EvaldGP :: String -> [Graphic CInt] -> Graphic CInt
    --At evaluation we floor at the end

--TODO CONSIDER A MORE TYPE SAFE AffineT WITH RESPECT TO COORDINATE SPACES
--TODO CONSIDER HOW TEXT CAN BE ADDED

anonGP = GroupPrim ""
anonEGP = EvaldGP ""
instance Show (Graphic Float) where
    show (Prim s) = "Prim " ++ show s
    show (GroupPrim label xs) = "GroupPrim "++ show label ++ show xs
    show (AffineT t s) = "AffineT " ++ show t ++ show s

instance Show (Graphic CInt) where
    show (EvaldP s) = "Evaluated Prim" ++ show s
    show (EvaldGP label gs) = "Evaluated GroupPrim" ++ show label ++ show gs
    show (AffineT _ _) = undefined --Evald specifically evaluates away AffineT's. TODO figure out a way to get this known

--TODO COLOR, EFFECTS, ANIMATIONS, SCENES LATER

-- | Time signal. Goes from 0 to 1, inclusive.
type Time = Float
-- | Duration of an animation or effect. Usually measured in seconds.
type Duration = Float

-- data Animation = Animation Duration (Time -> Graphic Shape)

-- staticFrame :: Duration -> Graphic Shape -> Animation
-- staticFrame d g = Animation d (const g)

-- | Evaluates the Shape Graphic and applies all the transformations
-- | Defaults the affine transformation to the identity matrix if the Graphic root isn't an AffineT
evalGraphic :: Graphic Float -> Graphic CInt
evalGraphic (AffineT t s) = evalGraphic' (coerce t) s
evalGraphic s = evalGraphic' m22AffineIdD s

-- | Aux that builds up the affine transformation as it recurses and applies once it hits the primitive
evalGraphic' :: M22Affine Float -> Graphic Float -> Graphic CInt
evalGraphic' t (Prim l) =  EvaldP $ applyAffineTransformFloor t l
evalGraphic' t (GroupPrim label gs) = EvaldGP label $ fmap (evalGraphic' t) gs
evalGraphic' t (AffineT t' s) = evalGraphic' (t !*! coerce t') s --TODO make sure this is the correct behavior when nesting transforms

{-

RE: Coerce usage here
 Couldn't match expected type `M22Affine Float'
                  with actual type `t'
      `t' is a rigid type variable bound by
        a pattern with constructor:
          AffineT :: forall t a.
                     (Coercible t (M22Affine Float), Show t) =>

Because we coerce phantom typed coordinate space transformations, we have coerce here too

This shit works because Data.Tagged is a newtype on M22Affine which is a type alias for V3 (V3 t)
-}
