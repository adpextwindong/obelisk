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
        bench "r1920 w10" $ nf (genRays 1920 p) 10,
        bench "r1024 w10" $ nf (genRays 1024 p) 10,
        bench "r640 w10" $ nf (genRays 640 p) 10,
        bench "r320 w10" $ nf (genRays 320 p) 10,
        bench "r240 w10" $ nf (genRays 240 p) 10,
        bench "r1920 w64" $ nf (genRays 1920 p) 64,
        bench "r1024 w64" $ nf (genRays 1024 p) 64,
        bench "r640 w64" $ nf (genRays 640 p) 64,
        bench "r320 w64" $ nf (genRays 320 p) 64,
        bench "r240 w64" $ nf (genRays 240 p) 64]
        ]