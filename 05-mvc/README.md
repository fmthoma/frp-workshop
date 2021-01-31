Structuring a GUI application: The MVC pattern
================================================================================

When applications get larger, putting everything in a single `setup` function is
just not very manageable – a phenomenon also known as »Spaghetti Code«.

To compensate this, a usual pattern is to split the application into Model, View
and Controller (MVC). In our case:

* *Model* is the application data and behavior. In case of the Counter, it's the
  `Int` type with the counter value, and the `increment` & `decrement`
  functions.

* *View* is the presentation of this data, in our cases the `Element`s.

* *Controller* is the connection between the model and the view. Controllers
  handle events from the view, update the model, and update the view with the
  changes from the model. In the case of FRP, the events and behaviors play the
  role of the controllers.

Although this tends to produce more code in the end, it is much more easily
accessible to others (or to yourself when coming back some time later), as you
can focus on changing a single part at a time.

The process of restructuring code for better readability and maintainability
without changing functionality is called _refactoring_. It is usually done in
small steps of doing a small change, recompiling and testing, to ensure that the
overall functionality stays the same.

Exercise
--------------------------------------------------------------------------------

Refactor the canvas exercise so that Model, View and Controller are clearly
separated.

### Hints

It is recommended to use new data types for the Model and the View:

Bad:

```haskell
canvas :: Element
button :: Element

-- No safeguard against passing the button here
renderCanvas :: Element -> UI ()
```

```haskell
counter :: Int
increment :: Int -> Int
```

Good:

```haskell
data Page = Page
    { canvas :: Element
    , counter :: Element
    , button :: Element }

-- RenderCanvas itself decides how to extract the canvas from the page
renderCanvas :: Page -> UI ()
```

```haskell
data ApplicationState = ApplicationState
    { counter :: Int }
incrementCounter :: ApplicationState -> ApplicationState
```

Also, booleans are not really good at conveying meaning – a problem also known
as »Boolean Blindness«:

```haskell
-- WTF are those bools?
renderText :: Bool -> Bool -> String -> UI ()
```

Haskell makes it easy to introduce Boolean-like data types that are
self-documenting and type-safe:

```haskell
-- Aaah now I see!
renderText :: Bold -> Italic -> String -> UI

data Bold = Bold | Normal
data Italic = Italic | Upright
```
