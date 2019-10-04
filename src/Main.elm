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
  { rater : Rater }


type Rater
  = Fixed Int
  | Transient Int Int


init : Model
init =
  { rater = Fixed 3 }


-- UPDATE


type Msg
  = MouseOver Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOver transientValue ->
      case model.rater of
        Fixed fixedValue ->
          { model | rater = Transient fixedValue transientValue }

        Transient fixedValue _ ->
          { model | rater = Transient fixedValue transientValue }


-- VIEW


view : Model -> Html Msg
view { rater } =
  let
    rating =
      case rater of
        Fixed fixedValue ->
          fixedValue

        Transient _ transientValue ->
          transientValue
  in
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
