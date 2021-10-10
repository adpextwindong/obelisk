mergeIntersections2

```haskell
import Data.Lists (mergeBy)

mergeIntersections2 playerpos xs ys = mergeBy (playerPointOrdering playerpos) xs ys

playerPointOrdering :: V2 Float -> V2 Float -> V2 Float -> Ordering
playerPointOrdering playerpos x y = if distance playerpos x < distance playerpos y
                                    then LT
                                    else GT
``

```
benchmarking rayCasting/rayCastScreen/r640 w64
time                 5.037 ms   (4.919 ms .. 5.174 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 4.944 ms   (4.924 ms .. 5.002 ms)
std dev              90.06 μs   (41.25 μs .. 181.7 μs)

benchmarking rayCasting/rayCastScreen/r320 w64
time                 2.510 ms   (2.497 ms .. 2.521 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.497 ms   (2.491 ms .. 2.504 ms)
std dev              19.76 μs   (16.77 μs .. 23.53 μs)

benchmarking rayCasting/rayCastScreen/r240 w64
time                 1.915 ms   (1.901 ms .. 1.932 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.939 ms   (1.929 ms .. 1.952 ms)
std dev              40.39 μs   (29.22 μs .. 54.92 μs)
```

mergeIntersections
```
benchmarking rayCasting/rayCastScreen/r640 w64
time                 5.086 ms   (5.009 ms .. 5.213 ms)
                     0.992 R²   (0.980 R² .. 0.999 R²)
mean                 5.058 ms   (4.995 ms .. 5.201 ms)
std dev              265.5 μs   (125.3 μs .. 459.1 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking rayCasting/rayCastScreen/r320 w64
time                 2.703 ms   (2.511 ms .. 2.957 ms)
                     0.975 R²   (0.960 R² .. 0.996 R²)
mean                 2.678 ms   (2.616 ms .. 2.783 ms)
std dev              260.0 μs   (173.1 μs .. 394.8 μs)
variance introduced by outliers: 66% (severely inflated)

benchmarking rayCasting/rayCastScreen/r240 w64
time                 2.015 ms   (1.922 ms .. 2.150 ms)
                     0.978 R²   (0.960 R² .. 1.000 R²)
mean                 1.988 ms   (1.961 ms .. 2.066 ms)
std dev              133.4 μs   (32.26 μs .. 234.3 μs)
```