module Obelisk.Math.Homogenous where

import Linear
import Control.Lens
import Foreign.C.Types ( CInt )
import Data.Bifunctor

type HV2 = V3 --TODO propagate this to indicate homoCoordsgenous coordinates
type Line = (HV2 CInt, HV2 CInt) -- After homoCoordsgenous coordinates its a v3


zoomT :: (Num a) => a -> V3 (V3 a)
zoomT scale = V3 (V3 scale 0     0)
                (V3 0     scale 0)
                (V3 0     0     1)

zoomAround :: Float -> V2 Float -> V3 (V3 Float)
zoomAround scale (V2 x y) = translate x y !*! zoomT scale !*! translate (-x) (-y)

translate :: (Num a) => a -> a -> V3 (V3 a)
translate x y = V3 (V3 1 0 x)
                   (V3 0 1 y)
                   (V3 0 0 1)

rotation :: Float -> V3 (V3 Float)
rotation theta = V3 (V3 (cos theta) (-sin theta) 0)
                    (V3 (sin theta) (cos theta)  0)
                    (V3  0           0           1)

rotation2 :: Float -> V2 (V2 Float)
rotation2 theta = V2 (V2 (cos theta) (-sin theta))
                    (V2 (sin theta) (cos theta))

rotateAround :: Float -> V2 Float -> V3 (V3 Float)
rotateAround theta (V2 x y) = translate x y !*! rotation theta !*! translate (-x) (-y)

type M22Affine t = V3 (V3 t) -- TODO use this type alias??

m22AffineIdD = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) :: M22Affine Float
idv3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) :: V3 (V3 CInt)

-- applyAffineTransform :: M22Affine CInt -> [Line] -> [Line]
-- applyAffineTransform t = fmap (bimap (t !* ) (t !*))

appDTFloor :: M22Affine Float -> [Line] -> [Line]
appDTFloor t = fmap (bimap f f)
    where f = floatTransformFloor t

floatTransformFloor :: RealFrac a => V3 (V3 a) -> V3 CInt -> V3 CInt
floatTransformFloor t = fmap floor . (t !*) . fmap fromIntegral :: V3 CInt -> V3 CInt

transformFloor t = fmap floor . (t !*)

-- foo t = fmap floor . (t !*) . fmap fromIntegral :: V3 CInt -> V3 CInt
--Convert to Floats, apply the transform then floor it

homoCoords :: (Num a) => V2 a -> HV2 a
homoCoords (V2 x y) = V3 x y 1

--TODO make sure the HV2 usage is correct
dropHomoCoords :: (Num a) => HV2 a -> V2 a
dropHomoCoords (V3 x y _) = V2 x y

apDT :: (Integral a) => M22Affine Float ->  HV2 Float -> V2 a
apDT t =  dropHomoCoords . fmap floor . (t !*)

gridT :: CInt -> Float -> Float -> Maybe (V2 Float) -> M22Affine Float -> M22Affine Float
gridT worldSize zoomFactor rotationFactor Nothing translateToPDCenter = translateToPDCenter !*! rotationT !*! zoomT zoomFactor !*! centerToLocalOrigin
    where
        delta = fromIntegral worldSize / 2
        centerToLocalOrigin = translate (-delta) (-delta) :: M22Affine Float
        rotationT = rotation rotationFactor

--Focuses onto something else in the world.
--TODO This should be taking in a viewport instead of all these params...
gridT worldSize zoomFactor rotationFactor (Just focus) translateToPDCenter = translateToPDCenter !*! zt !*! rotationT !*! playerToLocalOrigin
    where
        zt = zoomT zoomFactor
        wsMid = fromIntegral worldSize / 2.0
        playerToLocalOrigin = translate (-focus ^._x) (-focus ^._y)  :: M22Affine Float
        rotationT = rotation ((-pi/2.0) - rotationFactor)

translateToPDCenter :: CInt -> CInt -> M22Affine Float
translateToPDCenter screenWidth screenHeight = translate (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)


-- Grid To Player as center Local to Screen Affine Transformation
--The gtp
rawCenterScreenOnWorldGrid :: CInt -> CInt -> CInt -> M22Affine Float
rawCenterScreenOnWorldGrid ws screenWidth screenHeight = gridT ws zoomFactor rotationFactor focus tPDCenter
    where
    --TODO rotation focus mechanism
    -- let rotationFactor = bool 0.0 (vectorAngle. direction $ player gs) $ rotateToPView gs
    -- let focus = bool Nothing (Just (position . player $ gs)) $ rotateToPView gs
        zoomLimiter = 0.95
        zoomFactor = fromIntegral screenHeight / fromIntegral ws * zoomLimiter
        rotationFactor = 0.0
        focus = Nothing
        tPDCenter = translateToPDCenter screenWidth screenHeight
