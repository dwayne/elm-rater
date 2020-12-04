module Rater exposing (view)


import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events as E
import Rater.Rating as Rating exposing (Rating)


view : (Rating -> msg) -> Maybe (Int -> msg) -> Bool -> Rating -> Html msg
view onChange maybeOnHover clearable rating =
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
      List.indexedMap
        (\i -> viewSymbol onChange maybeOnHover clearable rating (i + 1))
        symbols
  in
  div
    [ style "display" "inline-block"
    , style "cursor" "pointer"
    ]
    viewSymbols


viewSymbol : (Rating -> msg) -> Maybe (Int -> msg) -> Bool -> Rating -> Int -> Html msg -> Html msg
viewSymbol onChange maybeOnHover clearable rating value symbol =
  let
    ratio =
      Rating.ratio rating

    newValue =
      if clearable && value == ratio.value then
        0
      else
        value

    onHoverAttr =
      case maybeOnHover of
        Nothing ->
          []

        Just onHover ->
          [ E.onMouseOver (onHover value) ]

    attrs =
      [ style "display" "inline-block"
      , E.onClick (onChange <| Rating.rate newValue rating)
      ] ++ onHoverAttr
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
