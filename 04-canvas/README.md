Using the HTML5 Canvas
================================================================================

After having learned the basics of FRP, let's turn to the graphical part of our
Snake implementation. For graphics rendering, plain HTML elements are just not
very good, so we use the HTML5 canvas element.

A canvas can be created with the [`canvas`] UI action. There's a dedicated
module [`Graphics.UI.Threepenny.Canvas`] with functions for painting on a
canvas. Interacting with a canvas is similar to other elements:

```haskell
myCanvas # set' fillStyle (htmlColor "black")
myCanvas # fillRect (0, 0) 20 20
```

Here we used [`fillStyle`] to first select a foreground color, and then
[`fillRect`] to draw a rectangle at the orginin (top-left corner) with a width
and height of 20 pixels, filled with the color we selected before. Not that for interacting with a canvas, you usually do not use `element`, and use `set'` instead of `set`.


[`canvas`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Elements.html#v:canvas
[`Graphics.UI.Threepenny.Canvas`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Canvas.html
[`fillStyle`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Canvas.html#v:fillStyle
[`fillRect`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Canvas.html#v:fillRect


Exercise
--------------------------------------------------------------------------------

This exercise basically starts from scratch:

1. Render a 400x400 px canvas with grey background
2. When the canvas is clicked, draw a 20x20 px black square in that location
3. Add a reset button to clear the canvas
4. Bonus: Allow drawing on the canvas by dragging the mouse with pressed button

### Hints

* The canvas uses floating point (`Double`) arithmetic, while we want to use
  integer (`Int`) arithmetic. To convert, use

        round, ceil, floor :: Double -> Int
        fromIntegral :: Int -> Double

* You don't get coordinates from click events, but you do from [`mousedown`],
  [`mouseup`] and [`mousemove`] events. These coordinates are relative to the
  element.

[`mousedown`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Events.html#v:mousedown
[`mouseup`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Events.html#v:mouseup
[`mousemove`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Events.html#v:mousemove
