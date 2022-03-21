# Obelisk

Wolf3D style rayCasting engine in progress. Architecture modeled after [DinoRush](https://github.com/jxv/dino-rush). (Thanks jxv!)

This project is meant to serve as a reference engine for teaching with as much of the core raycasting code as pure as possible. This way you can play with the code in GHCI to use as a reference for your own raycasting engine in another language.

## TODOS and Progress

### Raycasting

- [x] Get wall renderering working. [(mouseTestMain press f1 to see playerPOV)](res/wallcasting.mp4)
- [ ] Fix eye fix

### DEV BUILD
Nix and Nix-shell setup to make things more consistent

### CODE PAGE

Make sure [code page 65001](https://stackoverflow.com/a/25373117) is set. For some reason hPutChar in Criterion yakks cause codepage 437 is set. The .ghci file should handle it.

### Benching

Profiling raybench
```
./RayBench +RTS -p
```
```
Profiteur raybench.prof
```

TODO fprof-auto costcenters for Ray.hs
