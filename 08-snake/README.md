Snake
================================================================================

Now let's replace the single pixel by a snake that, just like the pixel, travels
in the current direction indicated by the directio behavior.

Keeping the MVC separation, we need to extend the model, and render the new
model in the view section. The controller is not affected by this change, since
we don't add any new events or behaviors!

Exercise:
--------------------------------------------------------------------------------

Instead of a single pixel, move a snake consisting of five pixels around.

### Hint

A Haskell pattern for looping over all items in a list inside a `do` block ( in
particular `IO` or `UI`) is using the [`for_`] function from [Data.Foldable]:

```haskell
for_ :: [a] -> (a -> IO b) -> IO ()
```

This can be used like a normal `foreach` statement:

```haskell
for_ pixels $ \pixel -> do
    doSomethingWith pixel
```

(More generally, [`for_`] works not just for lists, but for every `Foldable`.)

[Data.Foldable]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Foldable.html
[`for_`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Foldable.html#v:for_
