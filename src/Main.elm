module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events as Events


main : Program () Model Msg
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


type Msg
  = MouseOver Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOver value ->
      { model | rating = value }


-- VIEW


view : Model -> Html Msg
view { rating } =
  viewRater 5 rating


viewRater : Int -> Int -> Html Msg
viewRater total rating =
  div [ class "rater" ]
    (viewStars 1 rating selected ++ viewStars (rating + 1) total unselected)


viewStars : Int -> Int -> Html Msg -> List (Html Msg)
viewStars low high star =
  List.range low high
    |> List.map (viewStar star)


viewStar : Html Msg -> Int -> Html Msg
viewStar star value =
  div
    [ class "rater__star"
    , Events.onMouseOver (MouseOver value)
    ]
    [ star ]


selected : Html msg
selected =
  text "\u{2605}"


unselected : Html msg
unselected =
  text "\u{2606}"
