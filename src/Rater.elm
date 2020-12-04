module Rater exposing
  ( State
  , init
  , ActiveConfig, HoverHandlers
  , view, viewReadOnly
  )


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


type Config msg = Config
  { mode : Mode msg
  }


type Mode msg
  = Disabled
  | Enabled (Activity msg)


type Activity msg
  = ReadOnly
  | Active (ActiveConfig msg)


type alias ActiveConfig msg =
  { onChange : State -> Rating -> msg
  , hoverHandlers : Maybe (HoverHandlers msg)
  , clearable : Bool
  }


type alias HoverHandlers msg =
  { onHover : State -> Int -> msg
  , onLeave : State -> msg
  }


view : ActiveConfig msg -> State -> Rating -> Html msg
view config state rating =
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
        (\i -> viewSymbol config state rating (i + 1))
        symbols
  in
  div
    [ style "display" "inline-block"
    , style "cursor" "pointer"
    ]
    viewSymbols


viewSymbol : ActiveConfig msg -> State -> Rating -> Int -> Html msg -> Html msg
viewSymbol config state rating value symbol =
  let
    ratio =
      Rating.ratio rating

    newValue =
      if config.clearable && value == ratio.value then
        0
      else
        value

    hoverAttrs =
      case config.hoverHandlers of
        Nothing ->
          []

        Just handlers ->
          let
            transientRating =
              Rating.outOf ratio.maxValue value
          in
          [ E.onMouseOver (handlers.onHover (Transient transientRating) value)
          , E.onMouseOut (handlers.onLeave Permanent)
          ]

    attrs =
      [ style "display" "inline-block"
      , E.onClick (config.onChange Permanent <| Rating.rate newValue rating)
      ] ++ hoverAttrs
  in
  div attrs [ symbol ]


viewReadOnly : Rating -> Html msg
viewReadOnly rating =
  let
    ratio =
      Rating.ratio rating

    numFull =
      ratio.value

    numEmpty =
      ratio.maxValue - ratio.value

    symbols =
      (List.repeat numFull symbolFull) ++ (List.repeat numEmpty symbolEmpty)

    viewSymbols =
      List.map viewReadOnlySymbol symbols
  in
  div
    [ style "display" "inline-block"
    , style "cursor" "default"
    ]
    viewSymbols


viewReadOnlySymbol : Html msg -> Html msg
viewReadOnlySymbol symbol =
  div
    [ style "display" "inline-block" ]
    [ symbol ]


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
