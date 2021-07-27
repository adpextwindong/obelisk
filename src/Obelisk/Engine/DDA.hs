module Obelisk.Engine.DDA 
    where

import Linear

data DDAStep = NoWall | Step {
                            distX :: Double, -- Distance away from Ray Origin
                            distY :: Double
                        }

    deriving Show

intersectionPositions :: [DDAStep] -> [V2 Double]
intersectionPositions xs = [V2 x y | Step x y <- xs]

--FIXME, likely incorrect. We should sample from where the ray starts.
-- startingStep = Step 0.0 0.0

lengthStep :: DDAStep -> Double
lengthStep NoWall = read "Infinity"
lengthStep (Step x y) = x * x + y * y

step :: Double -> Double -> Double -> Double -> Bool -> DDAStep
step rise run x y inverted
    | run == 0.0 = NoWall
    | otherwise = Step nX nY
                    where dx = if run > 0
                                then fromIntegral (floor (x + 1.0)) - x
                                else fromIntegral (ceiling (x - 1.0)) - x
                          dy = dx * (rise / run)
                          nX = if inverted then y + dy else x + dx
                          nY = if inverted then x + dx else y + dy

-- Generates the path it takes through the scene
-- I want to decouple the map Inspection
rayPath :: Double -> DDAStep -> [(DDAStep, Double)]
rayPath _ NoWall = undefined --Should be unreachable because nextStep is guarenteed to pick a non NoWall step
rayPath angle origin@(Step x y) = (origin,offset) : rayPath angle nextStep
                                    where   stepX = step (sin angle) (cos angle) x y False
                                            stepY = step (cos angle) (sin angle) y x True
                                            nextStep = if lengthStep stepX < lengthStep stepY
                                                        then stepX
                                                        else stepY

                                            offset = if lengthStep stepX < lengthStep stepY
                                                        then offSetClip (distY stepX)
                                                        else offSetClip (distX stepY)

-- rayPath' :: Double -> [(DDAStep, Double)]
-- rayPath' angle = rayPath angle startingStep

offSetClip :: Double -> Double
offSetClip x = x - fromIntegral (floor x)

limitDrawDistance :: Double -> [DDAStep] -> [DDAStep]
limitDrawDistance drawDistance = takeWhile (\step -> sqrt (lengthStep step) < drawDistance)

limitDrawDistance' :: Double -> [(DDAStep, b)] -> [(DDAStep, b)]
limitDrawDistance' drawDistance = takeWhile (\(step, _) -> sqrt (lengthStep step) < drawDistance)

--Starting step should be ray origin
tx = fmap fst $ take 10 $ rayPath (pi/3) (Step 0 0)
ty = fmap fst $ take 10 $ rayPath (pi/3) (Step 0.25 0.33)