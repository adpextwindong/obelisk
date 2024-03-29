# Obelisk

Wolf3D style rayCasting engine in progress. Architecture modeled after [DinoRush](https://github.com/jxv/dino-rush). (Thanks jxv!)

This project is meant to serve as a reference engine for teaching with as much of the core raycasting code as pure as possible. This way you can play with the code in GHCI to use as a reference for your own raycasting engine in another language.

## Progress Demos

### Raycasting

- [x] Get wall renderering working.

- [x] Fish eye fix

https://user-images.githubusercontent.com/3671250/159427381-1f67245f-5bc1-4968-93ba-6950d17864f6.mp4

- [x] Wall Texturing

https://user-images.githubusercontent.com/3671250/160585571-89f21767-b303-42b2-bb90-cc9e78840af7.mp4

- [x] Multi Texture Selection

https://user-images.githubusercontent.com/3671250/161190527-e937a291-cfcf-4b03-be63-0faaa40a0f25.mp4

- [x] Transparent walls

![](res/alphaBlendAFTER.png)

### DEV BUILD
Nix and Nix-shell setup to make things more consistent

### Windows Build

Needed dlls

- SDL2.dll
- SDL2_ttf.dll
- libfreetype-6.dll
- zlib1.dll

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

Thank you to:

- [Fabien Sanglard](https://fabiensanglard.net/) for [Game Engine Black Book: Wolfenstein 3D](https://fabiensanglard.net/gebb/index.html)
