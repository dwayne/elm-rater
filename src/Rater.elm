module Rater exposing
  ( State
  , init

  , viewReadOnly, viewDisabled

  , Customizations, Orientation(..)
  , defaultCustomizations

  , viewCustomReadOnly, viewCustomDisabled

  , Config, HoverConfig
  , customConfig

  , viewSimple, viewClearable, viewHoverable
  , view

  , defaultSymbolEmpty, defaultSymbolFull
  )


import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events as E
import Rater.Rating as Rating exposing (Rating)


type State
  = Permanent
  | Transient Rating


init : State
init =
  Permanent


type Config msg =
  Config
    { mode : Mode msg
    , customizations : Customizations msg
    }


type Mode msg
  = ReadOnly
  | Disabled
  | Active (ActiveConfig msg)


type alias ActiveConfig msg =
  { onChange : Rating -> msg
  , maybeOnClear : Maybe msg
  , maybeHoverConfig : Maybe (HoverConfig msg)
  }


type alias HoverConfig msg =
  { state : State
  , onHover : State -> Int -> msg
  , onLeave : State -> msg
  }


type alias Customizations msg =
  { orientation : Orientation
  , symbolEmpty : Int -> Html msg
  , symbolFull : Int -> Html msg
  }


type Orientation
  = Horizontal
  | HorizontalReverse
  | Vertical
  | VerticalReverse


viewReadOnly : Rating -> Html msg
viewReadOnly =
  viewCustomReadOnly defaultCustomizations


viewCustomReadOnly : Customizations msg -> Rating -> Html msg
viewCustomReadOnly customizations =
  view (Config { mode = ReadOnly, customizations = customizations })


viewDisabled : Rating -> Html msg
viewDisabled =
  viewCustomDisabled defaultCustomizations


viewCustomDisabled : Customizations msg -> Rating -> Html msg
viewCustomDisabled customizations =
  view (Config { mode = Disabled, customizations = customizations })


defaultCustomizations : Customizations msg
defaultCustomizations =
  { orientation = Horizontal
  , symbolEmpty = always defaultSymbolEmpty
  , symbolFull = always defaultSymbolFull
  }


viewSimple : (Rating -> msg) -> Rating -> Html msg
viewSimple onChange rating =
  let
    config =
      customConfig
        { orientation = Horizontal
        , symbolEmpty = Nothing
        , symbolFull = Nothing
        , onChange = onChange
        , onClear = Nothing
        , hoverConfig = Nothing
        }
  in
  view config rating


viewClearable : (Rating -> msg) -> msg -> Rating -> Html msg
viewClearable onChange onClear rating =
  let
    config =
      customConfig
        { orientation = Horizontal
        , symbolEmpty = Nothing
        , symbolFull = Nothing
        , onChange = onChange
        , onClear = Just onClear
        , hoverConfig = Nothing
        }
  in
  view config rating


viewHoverable
  : { onChange : Rating -> msg
    , onClear : Maybe msg
    , onHover : State -> Int -> msg
    , onLeave : State -> msg
    }
  -> State
  -> Rating
  -> Html msg
viewHoverable options state rating =
  let
    config =
      customConfig
        { orientation = Horizontal
        , symbolEmpty = Nothing
        , symbolFull = Nothing
        , onChange = options.onChange
        , onClear = options.onClear
        , hoverConfig =
            Just
              { state = state
              , onHover = options.onHover
              , onLeave = options.onLeave
              }
        }
  in
  view config rating


customConfig
  : { orientation : Orientation
    , symbolEmpty : Maybe (Int -> Html msg)
    , symbolFull : Maybe (Int -> Html msg)
    , onChange : Rating -> msg
    , onClear : Maybe msg
    , hoverConfig : Maybe (HoverConfig msg)
    }
  -> Config msg
customConfig options =
  let
    mode =
      Active
        { onChange = options.onChange
        , maybeOnClear = options.onClear
        , maybeHoverConfig = options.hoverConfig
        }

    customizations =
      { orientation = options.orientation
      , symbolEmpty = Maybe.withDefault (always defaultSymbolEmpty) options.symbolEmpty
      , symbolFull = Maybe.withDefault (always defaultSymbolFull) options.symbolFull
      }
  in
  Config { mode = mode, customizations = customizations }


view : Config msg -> Rating -> Html msg
view (Config config) rating =
  case config.mode of
    ReadOnly ->
      viewHelper
        { orientation = config.customizations.orientation
        , symbolEmpty = config.customizations.symbolEmpty
        , symbolFull = config.customizations.symbolFull
        , maybeHoverConfig = Nothing
        , blockAttrs = [ class "elm-rater--read-only" ]
        , symbolAttrs = always []
        }
        rating

    Disabled ->
      viewHelper
        { orientation = config.customizations.orientation
        , symbolEmpty = config.customizations.symbolEmpty
        , symbolFull = config.customizations.symbolFull
        , maybeHoverConfig = Nothing
        , blockAttrs = [ class "elm-rater--disabled" ]
        , symbolAttrs = always []
        }
        rating

    Active activeConfig ->
      let
        changeConfig =
          { onChange = activeConfig.onChange
          , maybeOnClear = activeConfig.maybeOnClear
          }
      in
      viewHelper
        { orientation = config.customizations.orientation
        , symbolEmpty = config.customizations.symbolEmpty
        , symbolFull = config.customizations.symbolFull
        , maybeHoverConfig = activeConfig.maybeHoverConfig
        , blockAttrs = []
        , symbolAttrs =
            \value ->
              List.concat
                [ onClickAttrs changeConfig rating value
                , onHoverAttrs activeConfig.maybeHoverConfig rating value
                ]
        }
        rating


onClickAttrs
  : { onChange : Rating -> msg
    , maybeOnClear : Maybe msg
    }
  -> Rating
  -> Int
  -> List (Attribute msg)
onClickAttrs { onChange, maybeOnClear } rating value =
  let
    ratio =
      Rating.ratio rating
  in
    case maybeOnClear of
      Nothing ->
        if value == ratio.value then
          []
        else
          [ E.onClick (onChange <| Rating.rate value rating) ]

      Just onClear ->
        if value == ratio.value then
          [ E.onClick onClear ]
        else
          [ E.onClick (onChange <| Rating.rate value rating) ]


onHoverAttrs : Maybe (HoverConfig msg) -> Rating -> Int -> List (Attribute msg)
onHoverAttrs maybeHoverConfig rating value =
  let
    ratio =
      Rating.ratio rating
  in
  case maybeHoverConfig of
    Nothing ->
      []

    Just { onHover, onLeave } ->
      let
        transientRating =
          Rating.outOf ratio.maxValue value
      in
      [ E.onMouseOver (onHover (Transient transientRating) value)
      , E.onMouseOut (onLeave Permanent)
      ]


viewHelper
  : { orientation : Orientation
    , symbolEmpty : Int -> Html msg
    , symbolFull : Int -> Html msg
    , maybeHoverConfig : Maybe (HoverConfig msg)
    , blockAttrs : List (Attribute msg)
    , symbolAttrs : Int -> List (Attribute msg)
    }
  -> Rating
  -> Html msg
viewHelper options rating =
  let
    blockAttrs =
      List.concat
        [ [ class "elm-rater"
          , class (orientationClass options.orientation)
          ]
        , options.blockAttrs
        ]

    viewSymbols =
      List.indexedMap
        (viewSymbol options.symbolAttrs)
        (symbols symbolConfig maybeState rating)

    symbolConfig =
      { symbolEmpty = options.symbolEmpty
      , symbolFull = options.symbolFull
      }

    maybeState =
      case options.maybeHoverConfig of
        Nothing ->
          Nothing

        Just { state } ->
          Just state

  in
  div blockAttrs [ div [ class "elm-rater__symbols" ] viewSymbols ]


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


symbols
  : { symbolEmpty : Int -> Html msg
    , symbolFull : Int -> Html msg
    }
  -> Maybe State
  -> Rating
  -> List (Symbol msg)
symbols { symbolEmpty, symbolFull } maybeState rating =
  let
    currentRating =
      case maybeState of
        Nothing ->
          rating

        Just Permanent ->
          rating

        Just (Transient transientRating) ->
          transientRating

    ratio =
      Rating.ratio currentRating

    numFull =
      ratio.value

    numEmpty =
      ratio.maxValue - ratio.value
  in
  List.concat
    [ List.repeat numFull (Full symbolFull)
    , List.repeat numEmpty (Empty symbolEmpty)
    ]


viewSymbol : (Int -> List (Attribute msg)) -> Int -> Symbol msg -> Html msg
viewSymbol toAttrs index symbol =
  let
    value =
      index + 1

    (symbolType, toHtml) =
      case symbol of
        Empty s ->
          ("elm-rater__symbol--empty", s)

        Full s ->
          ("elm-rater__symbol--full", s)

    symbolAttrs =
      List.concat
        [ [ class "elm-rater__symbol", class symbolType ]
        , toAttrs value
        ]
  in
  div symbolAttrs [ toHtml value ]


defaultSymbolEmpty : Html msg
defaultSymbolEmpty =
  span [ class "elm-rater__star" ] [ text "\u{2606}" ]


defaultSymbolFull : Html msg
defaultSymbolFull =
  span [ class "elm-rater__star" ] [ text "\u{2605}" ]
