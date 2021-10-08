# Obelisk

Wolf3D style rayCasting engine in progress. Architecture modeled after [DinoRush](https://github.com/jxv/dino-rush). (Thanks jxv!)

This project is meant to serve as a reference engine for teaching with as much of the core raycasting code as pure as possible. This way you can play with the code in GHCI to use as a reference for your own raycasting engine in another language.

## TODOS

### Raycasting

Fix DDA code
Get wall renderering working

### Debug Tooling

REPL interface between GHCI and mainthread to add/remove/clear primitives in the world.
This probably needs looking into IOREF/STRef/MVar.

I'd like to simply mutate a stack of things I'm playing with so I can see if I'm creating correct drawPrimitive expressions correctly.

### DEV BUILD
Nix and Nix-shell setup to make things more consistent


### CODE PAGE

Make sure [code page 65001](https://stackoverflow.com/a/25373117) is set. For some reason hPutChar in Criterion yakks cause codepage 437 is set. The .ghci file should handle it.
