# Obelisk

Wolf3D style rayCasting engine in progress. Architecture modeled after [DinoRush](https://github.com/jxv/dino-rush). (Thanks jxv!)

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
