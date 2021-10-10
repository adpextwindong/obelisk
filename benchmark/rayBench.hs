import Criterion.Main

import Obelisk.Engine.Ray

import Linear.V2
import Obelisk.State
import Foreign.C.Types (CInt)
import GHC.IO.Encoding (setLocaleEncoding, getLocaleEncoding, utf8)

p = PVars (V2 2.5 6.5) dir cam
    where
        dir = V2 0.9304581537328835 (-0.36639817705888417)
        cam = V2 0.36639817705888417 0.9304581537328835

b = boxMap
b64 = boxMap64

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain [
        bgroup "rayCasting" [
            -- bgroup "genRays" [
            --     bench "r640 w10" $ nf (genRays 640 p) 10,
            --     bench "r320 w10" $ nf (genRays 320 p) 10,
            --     bench "r240 w10" $ nf (genRays 240 p) 10,

            --     bench "r640 w64" $ nf (genRays 640 p) 64,
            --     bench "r320 w64" $ nf (genRays 320 p) 64,
            --     bench "r240 w64" $ nf (genRays 240 p) 64],

            bgroup "rayCastScreen" [
                -- bench "r640 w10" $ nf (rayCastScreen 640 p) b,
                -- bench "r320 w10" $ nf (rayCastScreen 320 p) b,
                -- bench "r320 w10" $ nf (rayCastScreen 240 p) b,

                bench "r640 w64" $ nf (rayCastScreen 640 p) b64,
                bench "r320 w64" $ nf (rayCastScreen 320 p) b64,
                bench "r240 w64" $ nf (rayCastScreen 240 p) b64]
        ]]