module Rater exposing
  ( State
  , init

  , viewSimple, viewClearable, viewHoverable
  , viewReadOnly
  , viewDisabled

  , Orientation(..)

  , InactiveConfig
  , viewReadOnlyCustom
  , viewDisabledCustom

  , ActiveConfig
  , viewActiveCustom

  , Config(..)
  , viewCustom
  )


import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events as E
import Rater.Rating as Rating exposing (Rating)


type State
  = Permanent
  | Transient Rating


init : State
init =
  Permanent


type Orientation
  = Horizontal
  | HorizontalReverse
  | Vertical
  | VerticalReverse


type Config msg
  = ReadOnly (InactiveConfig msg)
  | Disabled (InactiveConfig msg)
  | Active (ActiveConfig msg)


type alias InactiveConfig msg =
  { orientation : Orientation
  , symbolEmpty : Int -> Html msg
  , symbolPermanent : Int -> Html msg
  }


defaultInactiveConfig : InactiveConfig msg
defaultInactiveConfig =
  { orientation = Horizontal
  , symbolEmpty = always defaultSymbolEmpty
  , symbolPermanent = always defaultSymbolPermanent
  }


type alias ActiveConfig msg =
  { onChange : Rating -> msg
  , maybeOnClear : Maybe msg
  , maybeHoverConfig : Maybe (HoverConfig msg)
  , orientation : Orientation
  , symbolEmpty : Int -> Html msg
  , symbolPermanent : Int -> Html msg
  }


type alias HoverConfig msg =
  { state : State
  , onHover : State -> Int -> msg
  , onLeave : State -> msg
  , symbolTransient : Int -> Html msg
  }


viewSimple : (Rating -> msg) -> Rating -> Html msg
viewSimple onChange rating =
  let
    activeConfig =
      { onChange = onChange
      , maybeOnClear = Nothing
      , maybeHoverConfig = Nothing
      , orientation = Horizontal
      , symbolEmpty = always defaultSymbolEmpty
      , symbolPermanent = always defaultSymbolPermanent
      }
  in
  viewActiveCustom activeConfig rating


viewClearable : (Rating -> msg) -> msg -> Rating -> Html msg
viewClearable onChange onClear rating =
  let
    activeConfig =
      { onChange = onChange
      , maybeOnClear = Just onClear
      , maybeHoverConfig = Nothing
      , orientation = Horizontal
      , symbolEmpty = always defaultSymbolEmpty
      , symbolPermanent = always defaultSymbolPermanent
      }
  in
  viewActiveCustom activeConfig rating


viewHoverable
  : { onChange : Rating -> msg
    , maybeOnClear : Maybe msg
    , onHover : State -> Int -> msg
    , onLeave : State -> msg
    }
  -> State
  -> Rating
  -> Html msg
viewHoverable { onChange, maybeOnClear, onHover, onLeave } state rating =
  let
    hoverConfig =
      { state = state
      , onHover = onHover
      , onLeave = onLeave
      , symbolTransient = always defaultSymbolTransient
      }

    activeConfig =
      { onChange = onChange
      , maybeOnClear = maybeOnClear
      , maybeHoverConfig = Just hoverConfig
      , orientation = Horizontal
      , symbolEmpty = always defaultSymbolEmpty
      , symbolPermanent = always defaultSymbolPermanent
      }
  in
  viewActiveCustom activeConfig rating


viewCustom : Config msg -> Rating -> Html msg
viewCustom config rating =
  case config of
    ReadOnly inactiveConfig ->
      viewReadOnlyCustom inactiveConfig rating

    Disabled inactiveConfig ->
      viewDisabledCustom inactiveConfig rating

    Active activeConfig ->
      viewActiveCustom activeConfig rating


viewActiveCustom : ActiveConfig msg -> Rating -> Html msg
viewActiveCustom activeConfig rating =
  let
    symbolConfig =
      case activeConfig.maybeHoverConfig of
        Nothing ->
          NoState
            { symbolEmpty = activeConfig.symbolEmpty
            , symbolPermanent = activeConfig.symbolPermanent
            }

        Just hoverConfig ->
          HasState
            hoverConfig.state
            { symbolEmpty = activeConfig.symbolEmpty
            , symbolPermanent = activeConfig.symbolPermanent
            , symbolTransient = hoverConfig.symbolTransient
            }

    viewSymbols =
      List.indexedMap
        (viewActiveSymbol activeConfig rating)
        (symbols symbolConfig rating)
  in
  div
    [ class "elm-rater"
    , class (orientationClass activeConfig.orientation)
    ]
    [ div [ class "elm-rater__symbols" ] viewSymbols ]


viewActiveSymbol : ActiveConfig msg -> Rating -> Int -> Symbol msg -> Html msg
viewActiveSymbol activeConfig rating index symbol =
  let
    value =
      index + 1

    ratio =
      Rating.ratio rating

    clickAttrs =
      case activeConfig.maybeOnClear of
        Nothing ->
          if value == ratio.value then
            []
          else
            [ E.onClick (activeConfig.onChange <| Rating.rate value rating) ]

        Just onClear ->
          if value == ratio.value then
            [ E.onClick onClear ]
          else
            [ E.onClick (activeConfig.onChange <| Rating.rate value rating) ]

    hoverAttrs =
      case activeConfig.maybeHoverConfig of
        Nothing ->
          []

        Just hoverConfig ->
          let
            transientRating =
              Rating.outOf ratio.maxValue value
          in
          [ E.onMouseOver (hoverConfig.onHover (Transient transientRating) value)
          , E.onMouseOut (hoverConfig.onLeave Permanent)
          ]
  in
  viewSymbol (clickAttrs ++ hoverAttrs) index symbol


viewReadOnly : Rating -> Html msg
viewReadOnly =
  viewReadOnlyCustom defaultInactiveConfig


viewReadOnlyCustom : InactiveConfig msg -> Rating -> Html msg
viewReadOnlyCustom inactiveConfig rating =
  let
    symbolConfig =
      NoState
        { symbolEmpty = inactiveConfig.symbolEmpty
        , symbolPermanent = inactiveConfig.symbolPermanent
        }
  in
  div
    [ class "elm-rater"
    , class (orientationClass inactiveConfig.orientation)
    , class "elm-rater--read-only"
    ]
    [ div [ class "elm-rater__symbols" ] <|
        List.indexedMap (viewSymbol []) (symbols symbolConfig rating)
    ]


viewDisabled : Rating -> Html msg
viewDisabled =
  viewDisabledCustom defaultInactiveConfig


viewDisabledCustom : InactiveConfig msg -> Rating -> Html msg
viewDisabledCustom inactiveConfig rating =
  let
    symbolConfig =
      NoState
        { symbolEmpty = inactiveConfig.symbolEmpty
        , symbolPermanent = inactiveConfig.symbolPermanent
        }
  in
  div
    [ class "elm-rater"
    , class (orientationClass inactiveConfig.orientation)
    , class "elm-rater--disabled"
    ]
    [ div [ class "elm-rater__symbols" ] <|
        List.indexedMap (viewSymbol []) (symbols symbolConfig rating)
    ]


orientationClass : Orientation -> String
orientationClass orientation =
  case orientation of
    Horizontal ->
      "elm-rater--horizontal"

    HorizontalReverse ->
      "elm-rater--horizontal-reverse"

    Vertical ->
      "elm-rater--vertical"

    VerticalReverse ->
      "elm-rater--vertical-reverse"


type Symbol msg
  = Empty (Int -> Html msg)
  | Full (Int -> Html msg)


type SymbolConfig msg
  = NoState
      { symbolEmpty : Int -> Html msg
      , symbolPermanent : Int -> Html msg
      }
  | HasState State
      { symbolEmpty : Int -> Html msg
      , symbolPermanent : Int -> Html msg
      , symbolTransient : Int -> Html msg
      }


symbols : SymbolConfig msg -> Rating -> List (Symbol msg)
symbols config rating =
  let
    (currentRating, symbolEmpty, symbolFull) =
      case config of
        NoState custom ->
          (rating, custom.symbolEmpty, custom.symbolPermanent)

        HasState Permanent custom ->
          (rating, custom.symbolEmpty, custom.symbolPermanent)

        HasState (Transient transientRating) custom ->
          (transientRating, custom.symbolEmpty, custom.symbolTransient)

    ratio =
      Rating.ratio currentRating

    numFull =
      ratio.value

    numEmpty =
      ratio.maxValue - ratio.value
  in
  List.repeat numFull (Full symbolFull) ++ List.repeat numEmpty (Empty symbolEmpty)


viewSymbol : List (Attribute msg) -> Int -> Symbol msg -> Html msg
viewSymbol attrs index symbol =
  let
    value =
      index + 1

    (symbolAttrs, s) =
      case symbol of
        Empty f ->
          ( [ class "elm-rater__symbol elm-rater__symbol--empty" ] ++ attrs
          , f value
          )

        Full f ->
          ( [ class "elm-rater__symbol elm-rater__symbol--full" ] ++ attrs
          , f value
          )
  in
  div symbolAttrs [ s ]


defaultSymbolEmpty : Html msg
defaultSymbolEmpty =
  span [ class "star" ] [ text "\u{2606}" ]


defaultSymbolPermanent : Html msg
defaultSymbolPermanent =
  span [ class "star" ] [ text "\u{2605}" ]


defaultSymbolTransient : Html msg
defaultSymbolTransient =
  defaultSymbolPermanent
