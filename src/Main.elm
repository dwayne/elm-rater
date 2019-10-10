module Main exposing (main)

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes as A


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
    (viewStars selected 1 rating ++ viewStars unselected (rating + 1) outOf)


viewStars : Html msg -> Int -> Int -> List (Html msg)
viewStars star low high =
  List.range low high
    |> List.map (always (viewStar star))


viewStar : Html msg -> Html msg
viewStar star =
  div [ A.class "rater__star-wrapper" ] [ star ]


selected : Html msg
selected =
  span [ A.class "rater__star" ] [ text "\u{2605}" ]


unselected : Html msg
unselected =
  span [ A.class "rater__star" ] [ text "\u{2606}" ]
