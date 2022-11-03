# November 2nd 2022

## Open Issues

- Skybox rendering
- Command architecture for inputs (+attack -attack like goldsrc). This should handle SDL's event stream semantics better.

```haskell

data CmdSign a = Pos a | Neg a

data Commands = CVar ...
              | SVar ...
              | UserInput (CmdSign Action)

data Action = Jump | Attack | Reload ...

gameTick :: Commands -> State -> ...
```

- Composable Input mapping (EX: Tab is always accessible for scoreboard but other inputs are state dependent)
- Dev Console
- OpenGL
- DearImgui

## Research

- Gabriella Gonazalez's bit on making a game and using the Continuation Monad
- Multithreading around SDL's graphics thread as main model.
