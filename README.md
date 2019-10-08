# Elm Rater

A reusable rater for Elm.

![A screenshot of example raters](/example-raters.png)

## Usage

**N.B.** *Work in progress. Not a library you can `elm install` as yet. But
feel free to reuse the code, [Rater](/src/Rater.elm) and
[Rater.Rating](/src/Rater/Rating.elm) as you see fit.*

```elm
import Rater
import Rater.Rating as Rating exposing (Rating)

-- ...

type alias Model =
  { rating : Rating
  , rater : Rater.State

  -- ...
  }

-- ...

init : Model
init =
  { rating = Rating.rate 3 5
  , rater = Rater.init

  -- ...
  }

-- ...

type Msg
  = NewRaterMsg Rater.Msg
  | ChangedRater Int
  -- ...

-- ...

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewRaterMsg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater model.rating raterMsg model.rater

        newModel =
          { model | rater = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    ChangedRater newAmount ->
      { model | rating = Rating.change newAmount model.rating }

    -- ...

-- ...

view : Model -> Html Msg
view model =
  div
    []
    [ Html.map NewRaterMsg (Rater.view model.rating model.rater) ]
```

## Demo

Check out the [demo](https://dwayne.github.io/elm-rater/) and view its
[code](/src/Main.elm) if you want to learn how to handle more advanced use
cases.

## Credits

The
[video of the API design session](https://www.youtube.com/watch?v=KSuCYUqY058)
that Evan and Greg recorded was an invaluable resource.

The following libraries also provided great working examples of how to approach
API design in Elm:

- [thebritican/elm-autocomplete](https://github.com/thebritican/elm-autocomplete)
- [NoRedInk/elm-sortable-table](https://github.com/NoRedInk/elm-sortable-table)
- [terezka/line-charts](https://github.com/terezka/line-charts)
