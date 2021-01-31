Workshop: GUI Programming in Haskell
================================================================================

[Threepenny-GUI] is a lightweight frontend framework for Haskell that showcases
the concepts of FRP. It uses the browser as a display, so Threepenny frontends
are portable, although it is not a fully-fledged web framework.


In order to use Threepenny, it is common to import two modules:

```haskell
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
```

That way, you can access the core functions and operators of
[Graphics.UI.Threepenny.Core] directly, while more specialized functions in
[Graphics.UI.Threepenny] are qualified with a `UI.` prefix, avoiding name
clashes.

Throughout this tutorial, the [Threepenny-GUI] documentation should come in
handy, in particular the modules [Graphics.UI.Threepenny.Core] and
[Reactive.Threepenny]. Other modules will be linked from the exercises as
required.

This workshop consists of ten exercises which build upon each other – the
solution of the previous exercise will be the basis for the next one. All
solutions are given, so feel free to start from the provided solution, or from
your own.

Before the workshop, you should run `stack build` once to build all the
exercises. For building and testing a single exercise, use

```
stack build :01-hello-world --exec 01-hello-world
```

and browse to [localhost:8023].

[Threepenny-GUI]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0
[Graphics.UI.Threepenny.Core]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html
[Graphics.UI.Threepenny]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny.html
[localhost:8023]: https://localhost:8023

Functional Reactive Programming
--------------------------------------------------------------------------------
