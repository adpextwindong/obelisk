import Criterion.Main

import Obelisk.Engine.Ray

import Linear.V2
import Obelisk.State
import Foreign.C.Types (CInt)

p = PVars (V2 2.5 6.5) dir cam
    where
        dir = V2 0.9304581537328835 (-0.36639817705888417)
        cam = V2 0.36639817705888417 0.9304581537328835

main :: IO ()
main = defaultMain [
    bgroup "genRays" [
        bench "r640" $ whnf (genRays 640 p) 10,
        bench "r320" $ whnf (genRays 320 p) 10,
        bench "r240" $ whnf (genRays 240 p) 10]
        ]