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
  | MouseOut
  | Clicked Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOver transientValue ->
      case model.rater of
        Fixed fixedValue ->
          { model | rater = Transient fixedValue transientValue }

        Transient fixedValue _ ->
          { model | rater = Transient fixedValue transientValue }

    MouseOut ->
      case model.rater of
        Fixed _ ->
          model

        Transient fixedValue _ ->
          { model | rater = Fixed fixedValue }

    Clicked newFixedValue ->
      let
        currentFixedValue =
          case model.rater of
            Fixed fixedValue ->
              fixedValue

            Transient fixedValue _ ->
              fixedValue
      in
        if newFixedValue == currentFixedValue then
          { model | rater = Fixed 0 }
        else
          { model | rater = Fixed newFixedValue }


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
  div
    [ class "rater"
    , Events.onMouseOut MouseOut
    ]
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
    , Events.onClick (Clicked value)
    ]
    [ star ]


selected : Html msg
selected =
  text "\u{2605}"


unselected : Html msg
unselected =
  text "\u{2606}"
