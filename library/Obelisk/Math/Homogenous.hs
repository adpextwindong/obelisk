module Obelisk.Math.Homogenous where

import Linear
import Foreign.C.Types ( CInt )
import Data.Bifunctor

type HV2 = V3 --TODO propagate this to indicate homoCoordsgenous coordinates
type Line = (HV2 CInt, HV2 CInt) -- After homoCoordsgenous coordinates its a v3


zoomT :: (Num a) => a -> V3 (V3 a)
zoomT scale = V3 (V3 scale 0     0)
                (V3 0     scale 0)
                (V3 0     0     1)

translate :: (Num a) => a -> a -> V3 (V3 a)
translate x y = V3 (V3 1 0 x)
                   (V3 0 1 y)
                   (V3 0 0 1)

rotation :: Double -> V3 (V3 Double)
rotation theta = V3 (V3 (cos theta) (-sin theta) 0)
                    (V3 (sin theta) (cos theta)  0)
                    (V3  0           0           1)

rotateAround :: Double -> V2 Double -> V3 (V3 Double)
rotateAround theta (V2 x y) = translate x y !*! rotation theta !*! translate (-x) (-y)

type M22Affine t = V3 (V3 t) -- TODO use this type alias??

idv3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) :: V3 (V3 CInt)

applyAffineTransform :: M22Affine CInt -> [Line] -> [Line]
applyAffineTransform t = fmap (bimap (t !* ) (t !*))

appDTFloor :: M22Affine Double -> [Line] -> [Line]
appDTFloor t = fmap (bimap f f)
    where
        f = fmap floor . (t !*) . fmap fromIntegral :: V3 CInt -> V3 CInt
        --Convert to doubles, apply the transform then floor it

homoCoords :: (Num a) => V2 a -> HV2 a
homoCoords (V2 x y) = V3 x y 1

--TODO make sure the HV2 usage is correct
dropHomoCoords :: (Num a) => HV2 a -> V2 a
dropHomoCoords (V3 x y _) = V2 x y
