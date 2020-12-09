# Elm Rater - [Live Demo](https://dwayne.github.io/elm-rater/)

A reusable rater for Elm.

[![A screenshot of example raters](/screenshot.png)](https://dwayne.github.io/elm-rater/)

## Usage

```elm
import Rater
import Rater.Rating as Rating exposing (Rating)
```

### Read Only and Disabled

```elm
type alias Model =
  { rating : Rating }

init =
  { rating = Rating.outOf5 3 }

view { rating } =
  div []
    [ Rater.viewReadOnly rating
    , Rater.viewDisabled rating
    ]
```

### Simple

Supports:

- click to change

```elm
type alias Model =
  { rating : Rating }

init =
  { rating = Rating.outOf5 3 }

type Msg
  = ChangedRating Rating

update msg model =
  case msg of
    ChangedRating newRating ->
      { model | rating = newRating }

view { rating } =
  div []
    [ Rater.viewSimple ChangedRating rating ]
```

### Clearable

Supports:

- click to change
- click to clear

```elm
type alias Model =
  { rating : Rating }

init =
  { rating = Rating.outOf5 3 }

type Msg
  = ChangedRating Rating
  | ClearedRating

update msg model =
  case msg of
    ChangedRating newRating ->
      { model | rating = newRating }

    ClearedRating ->
      { model | rating = Rating.zero model.rating }

view { rating } =
  div []
    [ Rater.viewClearable ChangedRating ClearedRating rating ]
```

### Hoverable

Supports:

- click to change
- click to clear
- mouse over
- mouse out

```elm
type alias Model =
  { rating : Rating
  , rater : Rater.State
  , transientValue : Maybe Int
  }

init =
  { rating = Rating.outOf5 3
  , rater = Rater.init
  , transientValue = Nothing
  }

type Msg
  = ChangedRating Rating
  | ClearedRating
  | HoveredOverRater Rater.State Int
  | LeftRater Rater.State

update msg model =
  case msg of
    ChangedRating newRating ->
      { model | rating = newRating }

    ClearedRating ->
      { model | rating = Rating.zero model.rating }

    HoveredOverRater state newTransientValue ->
      { model | rater = state, transientValue = Just newTransientValue }

    LeftRater state ->
      { model | rater = state, transientValue = Nothing }

view { rating, rater } =
  div []
    [ Rater.viewHoverable
        -- options
        { onChange = ChangedRating
        , onClear = Just ClearedRating
        , onHover = HoveredOverRater
        , onLeave = LeftRater
        }
        -- view state
        rater
        -- data
        rating
    , case transientValue of
        Nothing ->
          text "Hover over the rater to see this value change."

        Just value ->
          text <| String.fromInt value
    ]
```

### Custom

Customize the presets using one of the functions:

- `viewCustomReadOnly`
- `viewCustomDisabled`
- `viewCustomSimple`
- `viewCustomClearable`
- `viewCustomHoverable`

For e.g. suppose you wanted these features for your rater:

- click to change
- no click to clear
- mouse over
- mouse out
- hearts instead of stars

Then, you could use `viewCustomHoverable`.

```elm
type alias Model =
  { rating : Rating
  , rater : Rater.State
  }

init =
  { rating = Rating.outOf5 3
  , rater = Rater.init
  }

type Msg
  = ChangedRating Rating
  | HoveredOverRater Rater.State Int
  | LeftRater Rater.State

update msg model =
  case msg of
    ChangedRating newRating ->
      { model | rating = newRating }

    HoveredOverRater state _ ->
      { model | rater = state }

    LeftRater state ->
      { model | rater = state }

view { rating, rater } =
  div []
    [ Rater.viewCustomHoverable
        -- options
        { orientation = Rater.horizontal
        , symbols =
            Rater.sameSymbols <|
              \value ->
                span
                  [ class "elm-rater__heart"
                  , title <| String.fromInt value
                  ]
                  [ text "\u{2764}" ]
        , onChange = ChangedRating
        , onClear = Nothing
        , onHover = HoveredOverRater
        , onLeave = LeftRater
        }
        -- view state
        rater
        -- data
        rating
    ]
```

### Very Custom

For unadulterated customization check out the `Rater.customConfig` and
`Rater.view` functions.

## API Design Principles

These are some of the design principles I tried to put into practice.

### Single source of truth

The `Rater.Rating` type is a data structure that represents a rating. You pass
it to the various view functions provided by this library in order for it to be
presented on the page. However, **you maintain ownership of the value**.

*Check out the **You own the rating** example from the [demo](https://dwayne.github.io/elm-rater/).*

### Simple by default

It's designed to have a **smooth learning curve**. You can start by using the
simple functions and as your needs get more demanding you can switch to using
the more complex functions.

### Separate view state from the data

When you need support for hovering you will need to start using view state. The
view state, of type `Rater.State`, is a value that gets passed around in order
to keep track of what your mouse is currently over.

The view state is kept completely separate from the data.

Why?

Because the view state is only necessary to make the view work when hovering is
enabled. It's not needed otherwise. If you choose to present the rating
differently then you might not need view state or you may need a different type
of view state. In any case, **view state** is
**state you keep that is highly specific to the way you choose to present your data**.
It must **never mix with your data**.

### No custom `Msg` type

The views return `Html msg` and there is no need for an update function. The way
you end up using the library is similar to how you'd use `Html.input`. You
provide `msg` builders that wrap the values that need to be communicated.

```elm
Rater.viewCustomClearable
  : { orientation : ...
    , symbols : ...
    , onChange : Rating -> msg
    , onClear : msg
    }
  -> Rating
  -> Html msg

Rater.viewCustomClearable
  -- options
  { orientation = ...
  , symbols = ...
  , onChange = ChangedRating
  , onClear = ClearedRating
  }
  -- data
  rating

ChangedRating : Rating -> Msg
ClearedRating : Msg
```

### BEM

Use [BEM](https://dev.to/dwayne/my-notes-on-bem-4dni) to give the CSS a solid
structure.

Well structured HTML and CSS combined with Elm leads to bliss.

## Credits

Even though
[evancz/elm-sortable-table](https://github.com/evancz/elm-sortable-table)
is **DEPRECATED** it still has tremendous value for learning how to write
reusable views.
