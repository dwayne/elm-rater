module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


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
  viewRater 5 rating


viewRater : Int -> Int -> Html msg
viewRater total rating =
  div [ class "rater" ]
    (viewStars 1 rating selected ++ viewStars (rating + 1) total unselected)


viewStars : Int -> Int -> Html msg -> List (Html msg)
viewStars low high star =
  List.range low high
    |> List.map (always (viewStar star))


viewStar : Html msg -> Html msg
viewStar star =
  div [ class "rater__star" ] [ star ]


selected : Html msg
selected =
  text "\u{2605}"


unselected : Html msg
unselected =
  text "\u{2606}"
