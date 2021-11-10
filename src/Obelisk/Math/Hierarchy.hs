{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Obelisk.Math.Hierarchy where
import Obelisk.Math.Homogenous

-- | a and b represent the source coordinate system and target coordinate system the Affine transformation takes you through.
data M22CoordAffine t a b where
    M22CoordAffine :: M22Affine t -> M22CoordAffine t a b

data WorldC
data NDC
data PDC

data CoordinateSystem = 'CoordinateSystem

type family KCoordinateSystem' a where
    KCoordinateSystem' WorldC = 'CoordinateSystem
    KCoordinateSystem' NDC = 'CoordinateSystem
    KCoordinateSystem' PDC = 'CoordinateSystem

type KCoordinateSystem t = (KCoordinateSystem' t ~ 'CoordinateSystem)

-- | t is the underlying type for the V2/V3's to accomodate converting from Float to CInt eventually
-- | a is the coordinate system (enforced by Lift's ConstraintKind usage)
data HGraphic t a where
    LiftHT :: (KCoordinateSystem a, KCoordinateSystem b) => M22CoordAffine t a b -> HGraphic t (a -> b)
    ApTransform :: HGraphic t (a -> b) -> HGraphic t a -> HGraphic t b
    Compose :: HGraphic t (a -> b) -> HGraphic t (b -> c) -> HGraphic t (a -> c)

foo :: M22CoordAffine Float WorldC NDC
foo = M22CoordAffine m22AffineIdD :: M22CoordAffine Float WorldC NDC

bar :: M22CoordAffine Float NDC PDC
bar = M22CoordAffine m22AffineIdD :: M22CoordAffine Float NDC PDC

worldToPhys :: HGraphic Float (WorldC -> PDC)
worldToPhys = Compose (LiftHT foo) (LiftHT bar)

{- Does not type check
quux = Compose (LiftHT bar) (LiftHT foo)

• Couldn't match type ‘WorldC’ with ‘PDC’
  Expected type: HGraphic Float (PDC -> NDC)
    Actual type: HGraphic Float (WorldC -> NDC)
-}