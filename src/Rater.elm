module Rater exposing
  ( State
  , initial

  , UpdateConfig
  , defaultUpdateConfig

  , Msg
  , update, updateCustom

  , ViewConfig
  , defaultViewConfig

  , view, viewCustom
  , viewReadOnly, viewReadOnlyCustom
  , viewDisabled, viewDisabledCustom
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


type alias ViewConfig =
  { selected : Int -> Html Never
  , unselected : Int -> Html Never
  }


defaultViewConfig : ViewConfig
defaultViewConfig =
  { selected =
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
    value =
      case state of
        Fixed ->
          Rating.value rating

        Transient transientValue ->
          transientValue

    total =
      Rating.total rating
  in
    viewRater config total value


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


viewReadOnly : Rating -> Html Never
viewReadOnly =
  viewReadOnlyCustom defaultViewConfig


viewReadOnlyCustom : ViewConfig -> Rating -> Html Never
viewReadOnlyCustom { selected, unselected } rating =
  let
    value =
      Rating.value rating

    total =
      Rating.total rating
  in
    div [ class "rater is-read-only" ]
      (viewStars 1 value (viewReadOnlyStar selected) ++ viewStars (value + 1) total (viewReadOnlyStar unselected))


viewReadOnlyStar : (Int -> Html msg) -> Int -> Html msg
viewReadOnlyStar star value =
  div [ class "rater__star-wrapper" ] [ star value ]


viewDisabled : Rating -> Html Never
viewDisabled =
  viewDisabledCustom defaultViewConfig


viewDisabledCustom : ViewConfig -> Rating -> Html Never
viewDisabledCustom { selected, unselected } rating =
  let
    value =
      Rating.value rating

    total =
      Rating.total rating
  in
    div [ class "rater is-disabled" ]
      (viewStars 1 value (viewDisabledStar selected) ++ viewStars (value + 1) total (viewDisabledStar unselected))


viewDisabledStar : (Int -> Html msg) -> Int -> Html msg
viewDisabledStar star value =
  div [ class "rater__star-wrapper" ] [ star value ]


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
