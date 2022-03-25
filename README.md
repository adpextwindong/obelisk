# Obelisk

Wolf3D style rayCasting engine in progress. Architecture modeled after [DinoRush](https://github.com/jxv/dino-rush). (Thanks jxv!)

This project is meant to serve as a reference engine for teaching with as much of the core raycasting code as pure as possible. This way you can play with the code in GHCI to use as a reference for your own raycasting engine in another language.

## TODOS and Progress

### Raycasting

- [x] Get wall renderering working.

- [x] Fix eye fix

https://user-images.githubusercontent.com/3671250/159427381-1f67245f-5bc1-4968-93ba-6950d17864f6.mp4

- [ ] Wall Texturing

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

## Credits

Thank you to [Andrew Durdin (adurdin)](https://moddingwiki.shikadi.net/wiki/GameMaps_Format) for file format information.
