module Rater exposing
  ( State
  , initial

  , Msg
  , update

  , Mode(..)
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


update : Bool -> Maybe (Int -> msg) -> Maybe msg -> Msg -> State -> (State, Maybe msg)
update clearable onHover onLeave msg state =
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
          , onLeave
          )

        Transient fixedValue _ ->
          ( Fixed fixedValue
          , onLeave
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


type Mode
  = Enabled
  | ReadOnly
  | Disabled


view : Int -> Mode -> State -> Html Msg
view total mode state =
  let
    rating =
      case state of
        Fixed fixedValue ->
          fixedValue

        Transient _ transientValue ->
          transientValue
  in
    case mode of
      Enabled ->
        viewRater total rating

      ReadOnly ->
        viewReadOnlyRater total rating

      Disabled ->
        viewDisabledRater total rating


viewReadOnlyRater : Int -> Int -> Html msg
viewReadOnlyRater total rating =
  div [ class "rater is-read-only" ]
    (viewStars 1 rating (viewReadOnlyStar selected) ++ viewStars (rating + 1) total (viewReadOnlyStar unselected))


viewReadOnlyStar : Html msg -> Int -> Html msg
viewReadOnlyStar star n =
  div [ class "rater__star" ] [ star ]


viewDisabledRater : Int -> Int -> Html msg
viewDisabledRater total rating =
  div [ class "rater is-disabled" ]
    (viewStars 1 rating (viewDisabledStar selected) ++ viewStars (rating + 1) total (viewDisabledStar unselected))


viewDisabledStar : Html msg -> Int -> Html msg
viewDisabledStar star n =
  div [ class "rater__star" ] [ star ]


viewRater : Int -> Int -> Html Msg
viewRater total rating =
  div
    [ class "rater"
    , Events.onMouseOut MouseOut
    ]
    (viewStars 1 rating (viewStar selected) ++ viewStars (rating + 1) total (viewStar unselected))


viewStar : Html Msg -> Int -> Html Msg
viewStar star value =
  div
    [ class "rater__star"
    , Events.onMouseOver (MouseOver value)
    , Events.onClick (Clicked value)
    ]
    [ star ]


viewStars : Int -> Int -> (Int -> Html msg) -> List (Html msg)
viewStars low high starBuilder =
  List.range low high
    |> List.map starBuilder


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
