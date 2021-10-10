cabal build rayBench --enable-profiling

ghc-options:
    -O2
    -fprof-auto
    -prof
    "-with-rtsopts=-N -p -s -h -i0.1"
```
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