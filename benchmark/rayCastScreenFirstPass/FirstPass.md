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
