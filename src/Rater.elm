module Rater exposing
  ( State
  , initial

  , Msg
  , update

  , view
  )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events as Events


type State
  = Fixed Int
  | Transient Int Int


initial : Int -> State
initial value =
  Fixed value


type Msg
  = MouseOver Int
  | MouseOut
  | Clicked Int


update : Bool -> Maybe (Int -> msg) -> Msg -> State -> (State, Maybe msg)
update clearable onHover msg state =
  case msg of
    MouseOver transientValue ->
      let
        newState =
          case state of
            Fixed fixedValue ->
              Transient fixedValue transientValue

            Transient fixedValue _ ->
              Transient fixedValue transientValue
      in
        ( newState
        , maybeApply onHover transientValue
        )

    MouseOut ->
      case state of
        Fixed _ ->
          ( state
          , Nothing
          )

        Transient fixedValue _ ->
          ( Fixed fixedValue
          , Nothing
          )

    Clicked newFixedValue ->
      let
        currentFixedValue =
          case state of
            Fixed fixedValue ->
              fixedValue

            Transient fixedValue _ ->
              fixedValue
      in
        if clearable && newFixedValue == currentFixedValue then
          ( Fixed 0
          , Nothing
          )
        else
          ( Fixed newFixedValue
          , Nothing
          )


view : Int -> State -> Html Msg
view total state =
  let
    rating =
      case state of
        Fixed fixedValue ->
          fixedValue

        Transient _ transientValue ->
          transientValue
  in
    viewRater total rating


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


-- HELPERS


maybeApply : Maybe (a -> b) -> a -> Maybe b
maybeApply maybeF x =
  case maybeF of
    Nothing ->
      Nothing

    Just f ->
      Just (f x)
