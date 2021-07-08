module Obelisk.Engine.DDA (rayPath', limitDrawDistance, limitDrawDistance')
    where

data DDAStep = NoWall | Step {
                            distX :: Float, -- Distance away from Ray Origin
                            distY :: Float
                        }

    deriving Show

startingStep = Step 0.0 0.0

lengthStep :: DDAStep -> Float
lengthStep NoWall = read "Infinity"
lengthStep (Step x y) = x * x + y * y

step :: Float -> Float -> Float -> Float -> Bool -> DDAStep
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
rayPath :: Float -> DDAStep -> [(DDAStep, Float)]
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

rayPath' :: Float -> [(DDAStep, Float)]
rayPath' angle = rayPath angle startingStep

offSetClip :: Float -> Float
offSetClip x = x - (fromIntegral (floor x))

limitDrawDistance :: Float -> [DDAStep] -> [DDAStep]
limitDrawDistance drawDistance = takeWhile (\step -> sqrt (lengthStep step) < drawDistance)

limitDrawDistance' drawDistance = takeWhile (\(step, _) -> sqrt (lengthStep step) < drawDistance)
