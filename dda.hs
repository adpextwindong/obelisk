data DDAStep = NoWall | Step Float Float
    deriving Show
--TODO refactor Step to include distance from the origin


lengthStep :: DDAStep -> Float
lengthStep NoWall = read "Infinity"
lengthStep (Step x y) = x * x + y * y

l2d :: (Float, Float) -> Float
l2d (x,y) = x * x + y * y

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


--rayCast :: Player -> 
-- Generates the path it takes through the scene
-- I want to decouple the map Inspection
rayPath :: Float -> DDAStep -> [DDAStep]
rayPath _ NoWall = undefined --Should be unreachable because nextStep is guarenteed to pick a non NoWall step
rayPath angle origin@(Step x y) = origin : rayPath angle nextStep
                                    where   stepX = step (sin angle) (cos angle) x y False
                                            stepY = step (cos angle) (sin angle) y x True
                                            nextStep = if lengthStep stepX < lengthStep stepY
                                                        then stepX
                                                        else stepY

rayPathDrawDistanceLimited :: Float -> [DDAStep] -> [DDAStep]
rayPathDrawDistanceLimited drawDistance xs = takeWhile (\step -> sqrt (lengthStep step) < drawDistance) xs

type Map = [[Bool]]

-- Maybe we should just support heights
getMap :: (Float, Float) -> Map -> Maybe Bool 
getMap = undefined 
inspectMap :: [[Bool]] -> (Float, Float) -> Bool 
inspectMap = undefined