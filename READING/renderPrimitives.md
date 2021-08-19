Rebuilding the project everytime I want to change little rendering details is annoying.

If we had a stack to push/pop/clear containing 2d rendering primitives in painting order that would be nice.

Lens'ifying it would be sensible too.

```haskell
forM_ render stack
```

This issue I'm having reminds me of the free monad tax example where I should have an intermediate structure to look at. Directly blasting geometry across CFFI into the surface is dumb. I guess you could call it a VBO (Vertex Buffer Object).


SDL-gfx SDL-Primitive avalible calls
```
Pixel

Lines
    Line
    Smoothline
    HorizontalLine???
    VericalLine????
    thickline

Triangle
    Smooth
    Fill

Rects
    roundRectangle
    fillRectangle
    fillRoundRectangle

Curves
    Arc
    Circle
        circle
        SmoothCircle
        fillCircle
    Ellise
        ellipse
        smoothEllipse
        fillEllipse
    Pie
        Pie
        fillPie
    Bezier

polygon
    polygon
    smoothpolygon
    fillpolygon
```

Every draw call in SDL-primitive takes a color argument.
So given t we can traverse the geometry and evaluate the color.
We can have a constructor for color value functions thats just const in disguise to make it uniform.

Compositions:
```
Arrow Line Line Line
DoubleEnded Arrow
XMarks
Grid
```

Common actions
```
Color
Highlight/Lighten/Darken
Text annotation?
Affine Matrix Transformations

What if we had a hiearchy of Position types to denote Local space, world space, screen space
```

Things we want to do

```
Animations for things
Expr (t -> a)
Parameterize exprs over T
```
Look at how `[reanimate](https://github.com/reanimate/reanimate)` does things.



Traversable with sequence

```
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
```


We should make it nicer to apply transformations.

How would clipping/cropping go?

Idealy the geometry pipeline handles composition and transformations. Then it gets folded into a Effect.Renderer monad. If we treat it like a tree look a subtrees to see partial renders.

Damn... Looks like theres a [Kmett repo & comment related to this kind of stuff](https://www.reddit.com/r/haskell/comments/2xwd92/im_creating_a_functional_rendering_engine_in/cp4enhk/).
