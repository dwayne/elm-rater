module Rater exposing (State, init, view)


import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events as E
import Rater.Rating as Rating exposing (Rating)


type State
  = Permanent
  | Transient Rating


init : State
init =
  Permanent


view
  : (State -> Rating -> msg)
  -> Maybe (State -> Int -> msg)
  -> Maybe (State -> msg)
  -> Bool
  -> State
  -> Rating
  -> Html msg
view onChange maybeOnHover maybeOnLeave clearable state rating =
  let
    currentRating =
      case state of
        Permanent ->
          rating

        Transient transientRating ->
          transientRating

    ratio =
      Rating.ratio currentRating

    numFull =
      ratio.value

    numEmpty =
      ratio.maxValue - ratio.value

    symbols =
      (List.repeat numFull symbolFull) ++ (List.repeat numEmpty symbolEmpty)

    viewSymbols =
      List.indexedMap
        (\i -> viewSymbol onChange maybeOnHover maybeOnLeave clearable state rating (i + 1))
        symbols
  in
  div
    [ style "display" "inline-block"
    , style "cursor" "pointer"
    ]
    viewSymbols


viewSymbol
  : (State -> Rating -> msg)
  -> Maybe (State -> Int -> msg)
  -> Maybe (State -> msg)
  -> Bool
  -> State
  -> Rating
  -> Int
  -> Html msg
  -> Html msg
viewSymbol onChange maybeOnHover maybeOnLeave clearable state rating value symbol =
  let
    ratio =
      Rating.ratio rating

    newValue =
      if clearable && value == ratio.value then
        0
      else
        value

    hoverAttrs =
      case maybeOnHover of
        Nothing ->
          []

        Just onHover ->
          let
            transientRating =
              Rating.outOf ratio.maxValue value
          in
          [ E.onMouseOver (onHover (Transient transientRating) value) ]

    leaveAttrs =
      case maybeOnLeave of
        Nothing ->
          []

        Just onLeave ->
          [ E.onMouseOut (onLeave Permanent) ]

    attrs =
      [ style "display" "inline-block"
      , E.onClick (onChange Permanent <| Rating.rate newValue rating)
      ] ++ hoverAttrs ++ leaveAttrs
  in
  div attrs [ symbol ]


symbolEmpty : Html msg
symbolEmpty =
  span
    [ style "font-size" "48px"
    , style "color" "orange"
    ]
    [ text "\u{2606}" ]


symbolFull : Html msg
symbolFull =
  span
    [ style "font-size" "48px"
    , style "color" "orange"
    ]
    [ text "\u{2605}" ]
