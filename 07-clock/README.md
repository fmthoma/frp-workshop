Adding a game clock
================================================================================

So far, we've only moved the dot around by repeatedly pressing keys. Snake is a
game of beating the clock, but we don't have one yet.

The clock is our first external event, an event that is not triggered by the
user, but by the game itself. Such an external event can be created using
[`newEvent`]:

```haskell
-- eClock :: Event ()         The event
-- tClock :: () -> IO ()      A trigger for the event
(eClock, tClock) <- liftIO newEvent
```

[`newEvent`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:newEvent

Multi Threading
--------------------------------------------------------------------------------

The clock is completely independent from any user events, so it needs to be in a
separate thread. While there are a lot more elegant ways, the most primitive way
to fork an action in a separate thread is to use [`forkIO`] from the
[`Control.Concurrent`] package:

```haskell
liftIO $ forkIO $ forever $ do
    -- this runs in a separate thread
```

In that package, you can also find the [`threadDelay`] function. The
[`forever`] function is neat for repeating an action endlessly, exactly what we
want from a clock.

[`forkIO`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Concurrent.html#v:forkIO
[`Control.Concurrent`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Concurrent.html
[`threadDelay`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Concurrent.html#v:threadDelay
[`forever`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad.html#v:forever

Exercise
--------------------------------------------------------------------------------

Make the pixel move by itself on every clock tick. The arrow keys just change
direction. A clock speed of 2–3 ticks per second (0.3–0.4s per tick) is a good
value, but you can play around with different speeds.
