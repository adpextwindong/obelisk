# TODOS

## Diagnostics

- Graphics scene tree overhaul

```haskell
data Graphics a = GShape
                | GGroup [Graphics a]
                | GTexture?
                | GSurface?
                | GText?
                | Transformations...
                | Effects...
```

- eval :: Graphics a ->
- REPL for adding/removing/clearing graphics to the screen while we inspect things

# Animations
For now lets just have constant animations and figure out how evaluating animations will work later.


# Scenes
We need some sort of zipper to navigate scenes.

Input handling for navigating the Godbolt slide scenes.