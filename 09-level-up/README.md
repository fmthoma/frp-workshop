Level-Up
================================================================================

Next step in the snake game is to add an objective: We add an item (sometimes
also known as »edible« or »apple«) to the canvas. When the snake runs into the
item, the snake grows by one pixel, and a new item appears somewhere else on the
playing field.

Recursiveness: Tying the knot
--------------------------------------------------------------------------------

This is the first time that we base game behavior not just on _user_ input, but
on the game itself. That poses a problem: The game behavior
(`Behavior ApplicationState`) depends on itself!

```haskell
do
    -- ...
    let eLevelUp = somethingThatDependsOn bApp -- <-- not in scope, defined below
    let eApp = concatenate <$> unions [eStep, eReset, eChangeDirection, eLevelUp]
    bApp <- accumB initialState eApp
    -- ...
```

Looks impossible?

Due to Haskell's laziness, this kind of recursive definition is actually
possible. We just need to enable it by adding a language extension to our file
at the very top:

    {-# LANGUAGE RecursiveDo #-}

and replace `do` by `mdo`:

```haskell
mdo
    -- ...
    let eLevelUp = somethingThatDependsOn bApp -- <-- recursive!
    let eApp = concatenate <$> unions [eStep, eReset, eChangeDirection, eLevelUp]
    bApp <- accumB initialState eApp
    -- ...
```

In the documentation for [Reactive.Threepenny], there's a few more words on this
trick and its restrictions.

[Reactive.Threepenny]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Reactive-Threepenny.html#g:9

### How is this possible?

Recursive definitions are quite common in Haskell:

```haskell
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x: xs) = f x : map f xs  -- <-- referencing `map` from within its own definition
```

In `do` notation, however, we have statements that are evaluated one after the
other, so the statements down the line (potentially) depend on the results of
the actions above. This is, however, not true for `let` bindings: The values in
the `let` block do not depend on any side effects inside the `let` block, so
they can be used just as recursively as in the `map` example above.

For more information, you can have a look at the [GHC users guide].

[GHC users guide]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html

(Pseudo) Randomness
--------------------------------------------------------------------------------

It's very hard for computers to create truly random numbers, but it's easy to
create pseudo-random numbers: Given a single number (the _seed_), a computer can
create a sequence of numbers that _look_ random (in the sense that they follow a
uniform distribution), but are deterministic.

Since a random number generator is stateful (you need a new seed to generate a
new number), you either require `IO` for generating a new number, or you need to
explicitly pass the seed around:

```haskell
random :: Seed -> (Double, Seed)
```

```haskell
(randomNumber, newSeed) = random oldSeed
```

For our purposes, it's easier to use the [`randomRs`] function to generate an
infinite sequence of random numbers, and pull new numbers from that sequence
every time we need them:

```haskell
randomCoordinates = randomRs (0, 19) (mkStdGen 42)
```

where `(0, 19)` are the lower and upper bound (so we create integers between 0
and 19, inclusively), and 42 is the seed: We will always generate the same
sequence of pseudo-random numbers. That way, we avoid `IO` in our model
altogether.

[`randomRs`]: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html#v:randomRs

Exercise
--------------------------------------------------------------------------------

* Render a single green pixel (item) at a (pseudo-)random location. When the
  snake hits the item, it grows by one pixel, and the item moves to a new
  location.
* Keep track of the score, and display it next to the playing field.
