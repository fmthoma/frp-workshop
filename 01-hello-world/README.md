Hello World
================================================================================

The `UI` type
--------------------------------------------------------------------------------

Threepenny introduces the [`UI`] type for interacting with the browser. `UI` is
very similar to `IO`, just that it runs in the browser (on client side) rather
than on the server.

You can use typical `IO` notation (`do` notation) and functions (like
[`pure`]/[`return`], [`fmap`], etc.), and they work just the same with the `UI`
type.

In case you run into a function that requires `IO` instead of `UI`, you can use
[`liftIO`] in order to convert:

    putStrLn "Hello World"               :: IO ()
    liftIO (putStrLn "HelloWorld")       :: UI ()

[`UI`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#t:UI
[`pure`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:pure
[`return`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad.html#v:return
[`fmap`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad.html#t:Functor
[`liftIO`]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad-IO-Class.html#v:liftIO

UI elements
--------------------------------------------------------------------------------

A new blank element can be created with [`UI.new`].

To change an attribute of the element, you can use the [`(#)`] operator and the
[`set`] function – an example is given in [Graphics.UI.Threepenny].

All the elements you want to show need to be added to the body of the window
using the [`(#+)`] operator.

[`UI.new`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Elements.html#v:new
[`(#)`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:-35-
[`set`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:set
[`(#+)`]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny-Core.html#v:-35--43-
[Graphics.UI.Threepenny]: https://hackage.haskell.org/package/threepenny-gui-0.9.0.0/docs/Graphics-UI-Threepenny.html


Exercise
--------------------------------------------------------------------------------

Render an element with the text "Hello World"!

In order to try out your solution:
* Run `stack build :01-hello-world --exec 01-hello-world`
* Open [http://localhost:8023](http://localhost:8023) in your browser
