module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


main : Html msg
main =
  viewRater


-- VIEW


viewRater : Html msg
viewRater =
  div [ class "rater" ]
    [ viewStar selected
    , viewStar selected
    , viewStar selected
    , viewStar unselected
    , viewStar unselected
    ]


viewStar : Html msg -> Html msg
viewStar star =
  div [ class "rater__star" ] [ star ]


selected : Html msg
selected =
  text "\u{2605}"


unselected : Html msg
unselected =
  text "\u{2606}"
