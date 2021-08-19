Rebuilding the project everytime I want to change little rendering details is annoying.

If we had a stack to push/pop/clear containing 2d rendering primitives in painting order that would be nice.

Lens'ifying it would be sensible too.

```haskell
forM_ render stack
```

This issue I'm having reminds me of the free monad tax example where I should have an intermediate structure to look at. Directly blasting geometry across CFFI into the surface is dumb. I guess you could call it a VBO (Vertex Buffer Object).

```haskell
Lines
Arrow Line Line Line
Rects
Circles
Grid

We should make it nicer to apply transformations.

How would clipping/cropping go?

Idealy the geometry pipeline handles composition and transformations. Then it gets folded into a Effect.Renderer monad. If we treat it like a tree look a subtrees to see partial renders.

Damn... Looks like theres a [Kmett repo & comment related to this kind of stuff](https://www.reddit.com/r/haskell/comments/2xwd92/im_creating_a_functional_rendering_engine_in/cp4enhk/).
