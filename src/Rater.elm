module Rater exposing
  ( State
  , initial

  , UpdateConfig
  , defaultUpdateConfig

  , Msg
  , update, updateCustom

  , Mode(..)

  , ViewConfig
  , defaultViewConfig

  , view, viewCustom
  )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events as Events

import Rating exposing (Rating)


type State
  = Fixed
  | Transient Int


initial : State
initial =
  Fixed


type alias UpdateConfig msg =
  { clearable : Bool
  , onChange : Maybe (Int -> msg)
  , onHover : Maybe (Int -> msg)
  , onLeave : Maybe msg
  }


defaultUpdateConfig : UpdateConfig msg
defaultUpdateConfig =
  { clearable = True
  , onChange = Nothing
  , onHover = Nothing
  , onLeave = Nothing
  }


type Msg
  = MouseOver Int
  | MouseOut
  | Clicked Int
  | NoOp


update : Rating -> Msg -> State -> (State, Maybe msg)
update =
  updateCustom defaultUpdateConfig


updateCustom : UpdateConfig msg -> Rating -> Msg -> State -> (State, Maybe msg)
updateCustom config rating msg state =
  case msg of
    MouseOver transientValue ->
      ( Transient transientValue
      , maybeApply config.onHover transientValue
      )

    MouseOut ->
      ( Fixed
      , config.onLeave
      )

    Clicked newValue ->
      if newValue == Rating.value rating then
        ( Fixed
        , if config.clearable then
            maybeApply config.onChange 0
          else
            Nothing
        )
      else
        ( Fixed
        , maybeApply config.onChange newValue
        )

    NoOp ->
      ( state
      , Nothing
      )


type Mode
  = Enabled
  | ReadOnly
  | Disabled


type alias ViewConfig =
  { mode : Mode
  , selected : Int -> Html Never
  , unselected : Int -> Html Never
  }


defaultViewConfig : ViewConfig
defaultViewConfig =
  { mode = Enabled
  , selected =
      \_ ->
        span
          [ style "font-size" "48px"
          , style "color" "orange"
          ]
          [ text "\u{2605}" ]
  , unselected =
      \_ ->
        span
          [ style "font-size" "48px"
          , style "color" "orange"
          ]
          [ text "\u{2606}" ]
  }


view : Rating -> State -> Html Msg
view =
  viewCustom defaultViewConfig


viewCustom : ViewConfig -> Rating -> State -> Html Msg
viewCustom config rating state =
  let
    total =
      Rating.total rating

    value =
      case state of
        Fixed ->
          Rating.value rating

        Transient transientValue ->
          transientValue
  in
    case config.mode of
      Enabled ->
        viewRater config total value

      ReadOnly ->
        mapNeverToMsg (viewReadOnlyRater config total value)

      Disabled ->
        mapNeverToMsg (viewDisabledRater config total value)


viewReadOnlyRater : ViewConfig -> Int -> Int -> Html Never
viewReadOnlyRater { selected, unselected } total value =
  div [ class "rater is-read-only" ]
    (viewStars 1 value (viewReadOnlyStar selected) ++ viewStars (value + 1) total (viewReadOnlyStar unselected))


viewReadOnlyStar : (Int -> Html msg) -> Int -> Html msg
viewReadOnlyStar star value =
  div [ class "rater__star-wrapper" ] [ star value ]


viewDisabledRater : ViewConfig -> Int -> Int -> Html Never
viewDisabledRater { selected, unselected } total value =
  div [ class "rater is-disabled" ]
    (viewStars 1 value (viewDisabledStar selected) ++ viewStars (value + 1) total (viewDisabledStar unselected))


viewDisabledStar : (Int -> Html msg) -> Int -> Html msg
viewDisabledStar star value =
  div [ class "rater__star-wrapper" ] [ star value ]


viewRater : ViewConfig -> Int -> Int -> Html Msg
viewRater { selected, unselected } total value =
  div
    [ class "rater"
    , Events.onMouseOut MouseOut
    ]
    (viewStars 1 value (viewStar selected) ++ viewStars (value + 1) total (viewStar unselected))


viewStar : (Int -> Html Never) -> Int -> Html Msg
viewStar star value =
  div
    [ class "rater__star-wrapper"
    , Events.onMouseOver (MouseOver value)
    , Events.onClick (Clicked value)
    ]
    [ mapNeverToMsg (star value) ]


viewStars : Int -> Int -> (Int -> Html msg) -> List (Html msg)
viewStars low high starBuilder =
  List.range low high
    |> List.map starBuilder


-- HELPERS


mapNeverToMsg : Html Never -> Html Msg
mapNeverToMsg =
  Html.map (always NoOp)


maybeApply : Maybe (a -> b) -> a -> Maybe b
maybeApply maybeF x =
  case maybeF of
    Nothing ->
      Nothing

    Just f ->
      Just (f x)
