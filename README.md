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

Functional Reactive Programming (FRP) is a concept for writing reactive (or
asynchronous) programs in the paradigm of functional programming. »Reactive«
means that program execution is not driven by executing actions one after the
other, but rather by asynchronous events (such as user actions, or data points
travelling down a pipeline).

A more precise description of the core concepts of FRP is »Denotative
Continuous-Time Programming«:

* **Denotative semantics** do not focus on the individual steps to produce a result
    (»how to do it«), but rather on the result you want to achieve (»what to do«).
    Haskell itself is a good example of denotative programming:

    ```haskell
    -- sumOfSquares is the sum of the list with each element squared
    sumOfSquares xs = sum (map (^2) xs)
    ```

    The converse is **Operational semantics**, focusing on _how_ to do things. This
    is more prevalent in imperative languages such as C, C++, Java, …:

    ```java
    // sumOfSquares is obtained by starting with an initial partial sum of 0,
    // looping over all items, squaring the item, and adding it to the partial sum.
    // After the loop is done, the sum is returned.
    int sumOfSquares(int[] xs) {
        int sum = 0;
        for (int i = 0; i < xs.length; i++) {
            sum += x^2;
        }
        return sum;
    }
    ```

    FRP is denotative in the sense that it does not so much focus on _how_ to handle
    asynchronous events, but _what_ to do with the data.

* **Continuous-Time** programming adds to the denotative semantics with an
    explicit notion of time.

    Traditional reactive programming is all about handling events: Whenever an
    event occurs, we run some code, change some state, maybe trigger some new
    events, and that's it. There is no notion of time or behavior »in between
    events«.

    Continuous-Time programming adds the notion of _behaviors_: Continuous-Time
    functions that are defined at every point in time. Instead of changing state
    whenever an event occurs, for other events to register, events _produce_
    behaviors, which can be merged and mixed with other behaviors or events.

FRP has been originally described in a 1997 paper by Conal Elliott and Paul
Hudak. There are several Haskell libraries that follow this paradigm (e.g.
reactive-banana, Reflex), and the Elm programming language and framework is
largely based on the concepts.

Today, a large portion of single-page web applications are based on web
frameworks like React and Redux, or Vue.js, which are in turn based on the ideas
of FRP.
