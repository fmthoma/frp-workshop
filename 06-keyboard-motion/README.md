Keyboard motion
================================================================================

Our next step on the road towards a Snake implementation is listening to
keyboard events. These are registered on the HTML `body` element that you've
already using since the very first exercise.

There are two ways to register keys, either as `Char`s in the [`keypress`]
event, or as `KeyCode`s in the [`keydown`] event. Arrow keys are not represented
as characters, so you need to use the latter. To figure out which keycodes
belong to which key, you can just print the events to the console (don't forget
[`liftIO`]), or peek at the solution.

(Or you can just use WASD instead of arrow keys.)

[`keypress`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Events.html#v:keypress
[`keydown`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Events.html#v:keydown
[`liftIO`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad-IO-Class.html#v:liftIO

Listening to a behavior
--------------------------------------------------------------------------------

Since rendering on a canvas is not just an attribute change, we cannot just use
`sink` to link the behavior to an attribute, as we did before. In fact, since
we're changing the canvas all the time, it is now advisable to clear and re-draw
the canvas on every change.

In order to listen to changes of a behavior, use the [`onChanges`] function.
This function has to be used with a bit of caution: Behaviors can change
continuously, or even »change« without changing the value, so the handler may be
called much more often than you'd expect. Here's an example how you can use
[`onChanges`] safely:

```haskell
onChanges bApplication $ \appState -> render page appState
```

You should not use [`onChanges`] to trigger new events, or generally change
anything in the application state. In fact, handlers for [`onChanges`] should be
*idempotent*, i.e. calling the handler twice with the same input should not
change the state of your application in any way.

[`onChanges`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:onChanges

Exercise
--------------------------------------------------------------------------------

Starting from the previous exercise, render a single black pixel that can be
moved around with the arrow keys.

### Hints

Note we also need to change the behavior of the reset button: Rather than
clearing the state of the _view_ (i.e. the canvas), it should now clear the
state of the _model_ (i.e. reset the program itself the initial state).
