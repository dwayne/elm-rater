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


type State
  = Fixed Int
  | Transient Int Int


initial : Int -> State
initial value =
  Fixed value


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


update : Msg -> State -> (State, Maybe msg)
update =
  updateCustom defaultUpdateConfig


updateCustom : UpdateConfig msg -> Msg -> State -> (State, Maybe msg)
updateCustom config msg state =
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
        , maybeApply config.onHover transientValue
        )

    MouseOut ->
      case state of
        Fixed _ ->
          ( state
          , config.onLeave
          )

        Transient fixedValue _ ->
          ( Fixed fixedValue
          , config.onLeave
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
        if newFixedValue == currentFixedValue then
          if config.clearable then
            ( Fixed 0
            , maybeApply config.onChange 0
            )
          else
            ( state
            , Nothing
            )
        else
          ( Fixed newFixedValue
          , maybeApply config.onChange newFixedValue
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
  { total : Int
  , mode : Mode
  , selected : Int -> Html Never
  , unselected : Int -> Html Never
  }


defaultViewConfig : ViewConfig
defaultViewConfig =
  { total = 5
  , mode = Enabled
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


view : State -> Html Msg
view =
  viewCustom defaultViewConfig


viewCustom : ViewConfig -> State -> Html Msg
viewCustom config state =
  let
    rating =
      case state of
        Fixed fixedValue ->
          fixedValue

        Transient _ transientValue ->
          transientValue
  in
    case config.mode of
      Enabled ->
        viewRater config rating

      ReadOnly ->
        mapNeverToMsg (viewReadOnlyRater config rating)

      Disabled ->
        mapNeverToMsg (viewDisabledRater config rating)


viewReadOnlyRater : ViewConfig -> Int -> Html Never
viewReadOnlyRater { total, selected, unselected } rating =
  div [ class "rater is-read-only" ]
    (viewStars 1 rating (viewReadOnlyStar selected) ++ viewStars (rating + 1) total (viewReadOnlyStar unselected))


viewReadOnlyStar : (Int -> Html msg) -> Int -> Html msg
viewReadOnlyStar star value =
  div [ class "rater__star-wrapper" ] [ star value ]


viewDisabledRater : ViewConfig -> Int -> Html Never
viewDisabledRater { total, selected, unselected } rating =
  div [ class "rater is-disabled" ]
    (viewStars 1 rating (viewDisabledStar selected) ++ viewStars (rating + 1) total (viewDisabledStar unselected))


viewDisabledStar : (Int -> Html msg) -> Int -> Html msg
viewDisabledStar star value =
  div [ class "rater__star-wrapper" ] [ star value ]


viewRater : ViewConfig -> Int -> Html Msg
viewRater { total, selected, unselected } rating =
  div
    [ class "rater"
    , Events.onMouseOut MouseOut
    ]
    (viewStars 1 rating (viewStar selected) ++ viewStars (rating + 1) total (viewStar unselected))


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
