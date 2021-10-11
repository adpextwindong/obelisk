# Inspecting rayCastScreen for performance related details

## Overview

So far here is some initial benchmarking results of rayCastScreen on my home computer.

Things to note:

This was on an AMD Ryzen 7 3700x and rayCastScreen is single threaded. We still don't know if doing it in parallel for rays will be reasonable. Ideally we'll improve single threaded performance incase we ever target a lower spec machine.

```
cabal build rayBench --enable-profiling

ghc-options:
    -O2
    -fprof-auto
    -prof
    "-with-rtsopts=-N -p -s -h -i0.1"
benchmarking rayCasting/rayCastScreen/r640 w64
time                 7.502 ms   (7.467 ms .. 7.545 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.566 ms   (7.551 ms .. 7.585 ms)
std dev              47.10 μs   (35.86 μs .. 64.84s)μ

benchmarking rayCasting/rayCastScreen/r320 w64
time                 3.773 ms   (3.759 ms .. 3.790 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.768 ms   (3.759 ms .. 3.777 ms)
std dev              27.14 μs   (21.32 μs .. 35.37s)μ

benchmarking rayCasting/rayCastScreen/r240 w64
time                 2.862 ms   (2.847 ms .. 2.881 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.849 ms   (2.841 ms .. 2.861 ms)
std dev              32.18 μs   (26.22 μs .. 41.57s)μ
```

## Inspection of Prof using Profiteur

Some overviews using the prof and [profiteur](https://github.com/jaspervdj/profiteur).

![Time](1a580f0_Overview_Time.png)

![Alloc](1a580f0_Overview_Alloc.png)

- So far we can tell that mergeIntersection takes up half our time and allocates more than half the allocations.

- Clipworld takes up a chunk of time which could potentially be optimized away if we bound ray travel to the last grid intersection. That represents around 9% of time and 6% of allocs.

- Normalizing the ray could be lifted to ray generation time. This is 7.5% of time totalled across both x and y rayGridIntersections. 2% of allocs.

- Map access itself is only 5% of the runtime 3% of allocs!

## TODO

We need to look at strictness analysis to make sure we aren't building uneccesarily huge thunks.

Figure out what is causing these allocations in the core output.

Do these cheap optimizations on clipworld/normalizing.

Figure out if Kmett's [Linear library](https://hackage.haskell.org/package/linear-1.21.5/docs/Linear-V2.html) is optimizing as expected.

Now that we have a reference raygrid intersection implementation and a debug graphic dsl we could take a stab at DDA again.

Tips from [GHC User Guide](https://mpickering.github.io/ghc-docs/build-html/users_guide/sooner.html) that we could try:

Use stricness anotations
Use unboxed types
Inline Pragma

## Cheap Micro Optimizations

### Clipworld

So we can just bound the traversal to within the world grid instead of infinitely. We use Data.Sequence from `containers` to avoid rebuilding the step sequence and take a bounded amount in logn time.

Before:
```haskell
baseSteps = [0.0 ..]
```

```
time                 9.361 ms   (8.688 ms .. 10.08 ms)
                     0.978 R²   (0.971 R² .. 0.989 R²)
mean                 8.659 ms   (8.341 ms .. 9.600 ms)
std dev              1.421 ms   (618.3 μs .. 2.846 ms)
variance introduced by outliers: 77% (severely inflated)

benchmarking rayCasting/rayCastScreen/r320 w64
time                 4.501 ms   (4.165 ms .. 4.912 ms)
                     0.972 R²   (0.958 R² .. 0.990 R²)
mean                 4.484 ms   (4.386 ms .. 4.619 ms)
std dev              372.0 μs   (282.2 μs .. 515.9 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking rayCasting/rayCastScreen/r240 w64
time                 3.379 ms   (3.267 ms .. 3.510 ms)
                     0.986 R²   (0.965 R² .. 0.996 R²)
mean                 3.359 ms   (3.288 ms .. 3.478 ms)
std dev              285.8 μs   (169.9 μs .. 503.8 μs)
variance introduced by outliers: 57% (severely inflated)
```

Via profiteur:

```
clipWorld
Time	6.6
Alloc	6.7

rayCast'
Time	97.7
Alloc	99

```

After:

```haskell
baseStepsSeq :: Int -> Sq.Seq Float
baseStepsSeq worldSize = Sq.fromList [0 .. (fromIntegral worldSize)]

baseStepsBounded :: Int -> Sq.Seq Float -> Float -> Float ->  Sq.Seq Float
baseStepsBounded worldSize bss axisPosition axisRay = Sq.take upperBound bss
    where
        upperBound = if axisRay > 0
                     then floor $ fromIntegral worldSize - axisPosition
                     else floor axisPosition
```

```
benchmarking rayCasting/rayCastScreen/r640 w64
time                 12.73 ms   (12.65 ms .. 12.81 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.96 ms   (12.89 ms .. 13.05 ms)
std dev              213.8 μs   (169.3 μs .. 263.2 μs)

benchmarking rayCasting/rayCastScreen/r320 w64
time                 6.466 ms   (6.441 ms .. 6.488 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.510 ms   (6.490 ms .. 6.543 ms)
std dev              75.29 μs   (46.11 μs .. 117.3 μs)

benchmarking rayCasting/rayCastScreen/r240 w64
time                 4.843 ms   (4.814 ms .. 4.875 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.853 ms   (4.835 ms .. 4.876 ms)
std dev              60.81 μs   (42.27 μs .. 104.5 μs)

```

Yikes introduced a clear regression of 50% slow down (huge). Lets just not use Data.Sequence for now.

```
baseStepsSeq :: Int -> Sq.Seq Float
baseStepsSeq worldSize = Sq.fromList [0 .. (fromIntegral worldSize)]

baseStepsBounded :: Int -> Sq.Seq Float -> Float -> Float ->  Sq.Seq Float
baseStepsBounded worldSize bss axisPosition axisRay = Sq.take upperBound bss
    where
        upperBound = if axisRay > 0
                     then floor $ fromIntegral worldSize - axisPosition
                     else floor axisPosition
```

```haskell
baseSteps :: [Float]
baseSteps = [0.0 ..]

upperBound :: Int -> Float -> Float -> Int
upperBound worldSize axisPosition axisRay = if axisRay > 0
                                            then floor $ fromIntegral worldSize - axisPosition
                                            else floor axisPosition

baseStepsBounded :: Int -> Float -> Float -> [Float]
baseStepsBounded worldSize axisPosition axisRay = take (upperBound worldSize axisPosition axisRay) baseSteps
```

```
time                 8.294 ms   (8.242 ms .. 8.341 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.400 ms   (8.366 ms .. 8.441 ms)
std dev              112.4 μs   (93.85 μs .. 139.0 μs)

benchmarking rayCasting/rayCastScreen/r320 w64
time                 4.208 ms   (4.177 ms .. 4.240 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.192 ms   (4.178 ms .. 4.215 ms)
std dev              58.64 μs   (44.34 μs .. 82.98 μs)

benchmarking rayCasting/rayCastScreen/r240 w64
time                 3.133 ms   (3.122 ms .. 3.145 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.136 ms   (3.128 ms .. 3.145 ms)
std dev              29.79 μs   (23.48 μs .. 40.80 μs)
```

Prof file diff

Before:
```
COST CENTRE           MODULE             SRC                                         %time %alloc

mergeIntersections    Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(77,1)-(81,31)     44.9   56.0
xRayGridIntersections Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(37,1)-(41,71)      9.7   15.0
sampleWalkRayPaths    Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(84,1)-(91,58)      9.7    3.7
clipWorld             Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(103,1)-(104,64)    8.9    6.7
normalize             Linear.Metric      src\Linear\Metric.hs:(107,1)-(108,23)         7.4    2.3
yRayGridIntersections Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(45,1)-(49,71)      5.2    4.9
epsilonBump           Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(70,1)-(74,64)      4.9    6.3
accessMap             Obelisk.State      src\Obelisk\State.hs:31:1-81                  3.4    0.0
accessMapV            Obelisk.State      src\Obelisk\State.hs:28:1-39                  2.3    3.6
rayCastScreen         Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:133:1-105           1.0    0.3
```

After:
```
COST CENTRE           MODULE             SRC                                         %time %alloc

mergeIntersections    Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(88,1)-(92,31)    44.9   57.5
xRayGridIntersections Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(48,1)-(52,65)    11.7   14.4
sampleWalkRayPaths    Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(95,1)-(102,58)   10.2    3.6
epsilonBump           Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(81,1)-(85,64)     8.4    6.1
normalize             Linear.Metric      src\Linear\Metric.hs:(107,1)-(108,23)        5.4    2.2
baseStepsBounded      Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:45:1-108           4.6    6.2
accessMap             Obelisk.State      src\Obelisk\State.hs:31:1-81                 4.3    0.0
yRayGridIntersections Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(56,1)-(60,65)     3.9    4.7
accessMapV            Obelisk.State      src\Obelisk\State.hs:28:1-39                 3.7    3.5
```

Great now we can move onto lifting normalize.

Diff
```
 xRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
-xRayGridIntersections p r bss = (p +) . (*^ nr) <$> stepScales
+xRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
     where
-        nr = normalize r
         firstStep = abs $ deltaFirst (p^._x) (nr ^._x)
         stepScales = [(firstStep + x) / abs (nr ^._x) | x <- bss] --TODO unbound this once everything is kosher so it can scale to any worldsize
 
-
+--NOTE: These rays should be normalized
 yRayGridIntersections :: V2 Float -> V2 Float -> [Float] -> [V2 Float]
-yRayGridIntersections p r bss = (p +) . (*^ nr) <$> stepScales
+yRayGridIntersections p nr bss = (p +) . (*^ nr) <$> stepScales
     where
-        nr = normalize r
         firstStep = abs $ deltaFirst (p^._y) (nr ^._y)
         stepScales = [(firstStep + y) / abs (nr ^._y) | y <- bss] --TODO unbound this once everythign is kosher so it can scale to any worldsize

 rayHeads :: CInt -> PVars -> [V2 Float]
-rayHeads screenWidth player = [direction player + (camera_plane player ^* x) | x <- cameraPlaneSweep screenWidth] :: [V2 Float]
+rayHeads screenWidth player = [normalize (direction player + (camera_plane player ^* x)) | x <- cameraPlaneSweep screenWidth] :: [V2 Float]
```

After:
```
time                 7.760 ms   (7.715 ms .. 7.823 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.850 ms   (7.810 ms .. 7.931 ms)
std dev              156.1 μs   (98.23 μs .. 266.1 μs)

benchmarking rayCasting/rayCastScreen/r320 w64
time                 3.913 ms   (3.884 ms .. 3.944 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.926 ms   (3.905 ms .. 3.983 ms)
std dev              102.8 μs   (43.45 μs .. 210.8 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking rayCasting/rayCastScreen/r240 w64
time                 2.936 ms   (2.924 ms .. 2.946 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.940 ms   (2.932 ms .. 2.947 ms)
std dev              26.10 μs   (21.21 μs .. 35.20 μs)
```

```
COST CENTRE           MODULE             SRC                                        %time %alloc

mergeIntersections    Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(86,1)-(90,31)    51.8   59.3
xRayGridIntersections Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(48,1)-(51,65)    11.4   14.5
epsilonBump           Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(79,1)-(83,64)     9.0    6.3
sampleWalkRayPaths    Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(93,1)-(100,58)    8.6    3.7
accessMap             Obelisk.State      src\Obelisk\State.hs:31:1-81                 4.6    0.0
baseStepsBounded      Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:43:1-108           4.3    6.4
yRayGridIntersections Obelisk.Engine.Ray src\Obelisk\Engine\Ray.hs:(55,1)-(58,65)     4.1    4.4
accessMapV            Obelisk.State      src\Obelisk\State.hs:28:1-39                 3.4    3.6
```

