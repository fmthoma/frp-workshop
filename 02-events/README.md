Events
================================================================================

Now let's start with one of the building blocks of Functional Reactive
Programming: Events.

The [`Event`] type in FRP represents a stream of events as they occur in time.
You can think of an event as a discrete map from a point in time to a value,
`Map Time a`, i.e. it only has a defined value at certain points in time.

You can listen to events using the [`on`] and [`onEvent`] functions, supplying a
callback:

```haskell
on UI.click element $ \() -> do
    -- handle the event

onEvent eClick $ \() -> do
    -- handle the event
```

The callback, a UI action, will be executed every time the event occurs.

A full list of events that can be triggered by elements can be found in the
[Graphics.UI.Threepenny.Events] module.

[`Event`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Reactive-Threepenny.html#t:Event
[`on`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:on
[`onEvent`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:onEvent
[Graphics.UI.Threepenny.Events]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Events.html

Exercise
--------------------------------------------------------------------------------

Render a button that changes its text from »Click Me« to »Thanks« when you click
it.

### Hint

If you get error messages along the lines of »Couldn't match type ‘Element’ with
‘UI Element’«, then you probably forgot to put [`element`] in front of an
element. Try

    element foo # set UI.text "Foo"

instead of

    foo # set UI.text "Foo"

[`element`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:element
