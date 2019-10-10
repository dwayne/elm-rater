module Main exposing (main)

import Html exposing (Html, div, span, text)
import Html.Attributes as A


main : Html msg
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
