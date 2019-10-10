# Section 1

## Goal

To display 5 stars.

## Plan

- Display 5 stars
- Prepare for interactivity
- Refactor `viewXStars`

## Display 5 stars

We'll start from a simple hello world app.

Edit `src/Main.elm`:

```elm
import Html exposing (Html, div, span, text)
import Html.Attributes as A


main =
  viewRater


-- VIEW


viewRater : Html msg
viewRater =
  div [ A.class "rater" ]
    [ viewStar selected
    , viewStar selected
    , viewStar selected
    , viewStar unselected
    , viewStar unselected
    ]


viewStar : Html msg -> Html msg
viewStar star =
  div [ A.class "rater__star-wrapper" ] [ star ]


selected : Html msg
selected =
  span [ A.class "rater__star" ] [ text "\u{2605}" ]


unselected : Html msg
unselected =
  span [ A.class "rater__star" ] [ text "\u{2606}" ]
```

Edit `index.html`:

```html
<!doctype html>
<html lang="en">
  <head>
    <!-- ... -->

    <style>
      .rater {
        display: inline-block;
        cursor: pointer;
      }

      .rater__star-wrapper {
        display: inline-block;
      }

      .rater__star {
        font-size: 48px;
        color: orange;
      }
    </style>
  </head>
  <body>
    <!-- ... -->
  </body>
</html>
```

## Prepare for interactivity

We have to start using `Browser.sandbox` so that we can handle the various
events that could occur on the rater.

Edit `src/Main.elm`:

```elm
import Browser


main : Program () Model msg
main =
    Browser.sandbox
      { init = init
      , update = update
      , view = view
      }


-- MODEL


type alias Model =
  { rating : Int }


init : Model
init =
  { rating = 3 }


-- UPDATE


update : msg -> Model -> Model
update _ = identity


-- VIEW


view : Model -> Html msg
view { rating } =
  viewRater rating 5


viewRater : Int -> Int -> Html msg
viewRater rating outOf =
  div [ A.class "rater" ]
    (viewSelectedStars 1 rating ++ viewUnselectedStars (rating + 1) outOf)


viewSelectedStars : Int -> Int -> List (Html msg)
viewSelectedStars low high =
  List.range low high
    |> List.map (always (viewStar selected))


viewUnselectedStars : Int -> Int -> List (Html msg)
viewUnselectedStars low high =
  List.range low high
    |> List.map (always (viewStar unselected))
```

## Refactor `viewXStars`

`viewSelectedStars` and `viewUnselectedStars` can be refactored into a
`viewStars` function.

Edit `src/Main.elm`:

```elm
viewRater : Int -> Int -> Html msg
viewRater rating outOf =
  div [ A.class "rater" ]
    (viewStars selected 1 rating ++ viewStars unselected (rating + 1) outOf)


viewStars : Html msg -> Int -> Int -> List (Html msg)
viewStars star low high =
  List.range low high
    |> List.map (always (viewStar star))
```
