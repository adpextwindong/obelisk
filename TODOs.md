# TODOS

"Every good game is a potemkin village..."

## CORE

- [ ] Finish GEBB/ [Permadi](https://permadi.com/1996/05/ray-casting-tutorial-table-of-contents/)
  - [ ] Wall Height Scaling
  - [ ] Test harness for h=x/d X "simple" scaling factor. See GEBB page 136
  - [ ] Wall Painting
  - [ ] Fish Eye Handling

## Figures

### Godbolt

- [ ] Title
- [ ] Wolf3D Image
- [ ] Raycasting
  - [ ] Gif Support?
- [ ] Map Viewport
  - [ ] Clear Screen
  - [ ] FOV Bits
- [ ] Rendering One Wall at a Time
- [ ] Intersection Figures
  - [ ] Intersection Point
  - [ ] Ray Figure
  - [ ] Grid Traversal
    - [ ] Y and X Intersections
    - [ ] Wall Hit Highlighting
  - [ ] δy and δx spans
- [ ] Math Slides
  - [ ] There are few angle arcs that need to be drawn

### Gebb

- [ ] [4.7.2 - Life of a 3D Frame Figure 4.22: Stage 1: Clear screen](https://fabiensanglard.net/gebbwolf3d.pdf#page=140)

## Test Harnesses

[ ] Test harness for h=x/d X "simple" scaling factor. See GEBB page 136
[ ] FOV. See [Lodev](https://lodev.org/cgtutor/raycasting.html)
[ ] Console REPL for adding/removing/clearing graphics to the screen while we inspect things

### Text

[ ] GText which represents a simple point in world/screenspace et
[ ] Caching of Text Surfaces

Really not sure if text layout handling is needed for. We could probably distinguish between static and dynamic text to make caching more convinient.

## Animations

For now lets just have constant animations and figure out how evaluating animations will work later.

## Scenes

We need some sort of zipper to navigate scenes.

[x] List Zipper Import and Usage for swtiching between scenes
[x] Some notion of scene so we can do in engine examples

Input handling for navigating the Godbolt slide scenes.

## Keyboard Input

[ ] Composable keyboard input controls for specific scenes (up/down arrow specifically in the godbolt examples)
    [ ] For any scenes with a viewport into the game it would be nice to retain regular gameplay controls with the harness controls mapped ontop.

### Keystate handling

Figure out how JXV's library is really supposed to be used.

## Experimental Ideas

- [x] Test out annotating matrix/vector types with what space they are in
  - [x] Have the graphics dsl AffineT :: M22Affine a -> Graphic (a -> b), composition operator for (a -> b) -> (b -> c) -> (a -> c)

It would be nice if anything touching the graphics dsl pipeline has a notion of which coordinate system its in. The vector math might become too unwieldy to do something like that. But anything ready to be composed for rendering should have that notion.

```
Local -> Object -> World  -> Normalized Device Coordinates -> Physical Device Coordinates
```

This idea might be more coerce's than I'd like.
