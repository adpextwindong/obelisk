{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Obelisk.Math.Hierarchy where

import Obelisk.Math.Homogenous

data M22CoordAffine t (a :: * -> *) (b :: * -> *) where
    M22CoordAffine :: M22Affine t -> M22CoordAffine t a b

data CoordinateSystem t = 'WorldC t | 'NormalizedDevicedC t | 'PhysicalDeviceC t

testM22 :: M22Affine Float
testM22 = undefined

--Yeah this shit sucks, lets try Proxy instead. I have no clue how to instantiate a type signature that fits.
-- test = M22CoordAffine testM22 :: M22CoordAffine Float ('WorldC Float) ('PhysicalDeviceC)

data HGraphic t a where
    ApTransform :: HGraphic t (a t -> b t) -> HGraphic t (a t) -> HGraphic t (b t)
    FunnyAffineT :: M22CoordAffine t a b -> HGraphic t (a t -> b t)
