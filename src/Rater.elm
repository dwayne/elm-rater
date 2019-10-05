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


update : Int -> Msg -> State -> (State, Maybe msg)
update =
  updateCustom defaultUpdateConfig


updateCustom : UpdateConfig msg -> Int -> Msg -> State -> (State, Maybe msg)
updateCustom config rating msg state =
  case msg of
    MouseOver transientRating ->
      ( Transient transientRating
      , maybeApply config.onHover transientRating
      )

    MouseOut ->
      ( Fixed
      , config.onLeave
      )

    Clicked newRating ->
      if newRating == rating then
        ( Fixed
        , if config.clearable then
            maybeApply config.onChange 0
          else
            Nothing
        )
      else
        ( Fixed
        , maybeApply config.onChange newRating
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


view : Int -> State -> Html Msg
view =
  viewCustom defaultViewConfig


viewCustom : ViewConfig -> Int -> State -> Html Msg
viewCustom config rating state =
  let
    ratingToUse =
      case state of
        Fixed ->
          rating

        Transient transientRating ->
          transientRating
  in
    case config.mode of
      Enabled ->
        viewRater config ratingToUse

      ReadOnly ->
        mapNeverToMsg (viewReadOnlyRater config ratingToUse)

      Disabled ->
        mapNeverToMsg (viewDisabledRater config ratingToUse)


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
