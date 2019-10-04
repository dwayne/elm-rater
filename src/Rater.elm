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

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
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


type Mode
  = Enabled
  | ReadOnly
  | Disabled


type alias ViewConfig =
  { total : Int
  , mode : Mode
  }


defaultViewConfig : ViewConfig
defaultViewConfig =
  { total = 5
  , mode = Enabled
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
        viewRater config.total rating

      ReadOnly ->
        viewReadOnlyRater config.total rating

      Disabled ->
        viewDisabledRater config.total rating


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
