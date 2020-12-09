module Rater exposing
  ( State
  , init

  , viewReadOnly, viewDisabled
  , viewSimple, viewClearable, viewHoverable

  , Orientation
  , horizontal, horizontalReverse, vertical, verticalReverse
  , defaultOrientation

  , SymbolsCustomization
  , sameSymbols, differentSymbols
  , defaultSymbols, defaultSymbolEmpty, defaultSymbolFull

  , viewCustomReadOnly, viewCustomDisabled
  , viewCustomSimple, viewCustomClearable, viewCustomHoverable

  , Customization
  , defaultCustomization

  , Config, HoverConfig
  , customConfig

  , view
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


-- ORIENTATION


type Orientation
  = Horizontal
  | HorizontalReverse
  | Vertical
  | VerticalReverse


horizontal : Orientation
horizontal =
  Horizontal


horizontalReverse : Orientation
horizontalReverse =
  HorizontalReverse


vertical : Orientation
vertical =
  Vertical


verticalReverse : Orientation
verticalReverse =
  VerticalReverse


defaultOrientation : Orientation
defaultOrientation =
  Horizontal


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


-- SYMBOL


type SymbolsCustomization msg
  = Same (Int -> Html msg)
  | Different
      { symbolEmpty : Int -> Html msg
      , symbolFull : Int -> Html msg
      }


sameSymbols : (Int -> Html msg) -> SymbolsCustomization msg
sameSymbols =
  Same


differentSymbols
  : { symbolEmpty : Int -> Html msg
    , symbolFull : Int -> Html msg
    }
  -> SymbolsCustomization msg
differentSymbols =
  Different


defaultSymbols : SymbolsCustomization msg
defaultSymbols =
  differentSymbols
    { symbolEmpty = always defaultSymbolEmpty
    , symbolFull = always defaultSymbolFull
    }


defaultSymbolEmpty : Html msg
defaultSymbolEmpty =
  span [ class "elm-rater__star" ] [ text "\u{2606}" ]


defaultSymbolFull : Html msg
defaultSymbolFull =
  span [ class "elm-rater__star" ] [ text "\u{2605}" ]


-- CUSTOMIZATION


type Customization msg =
  Customization
    { orientation : Orientation
    , symbols : SymbolsCustomization msg
    }


defaultCustomization : Customization msg
defaultCustomization =
  Customization
    { orientation = defaultOrientation
    , symbols = defaultSymbols
    }


-- CONFIG


type Config msg =
  Config
    { mode : Mode msg
    , customization : Customization msg
    }


type Mode msg
  = ReadOnly
  | Disabled
  | Active (ActiveConfig msg)


type alias ActiveConfig msg =
  { onChange : Rating -> msg
  , onClear : Maybe msg
  , hoverConfig : Maybe (HoverConfig msg)
  }


type alias HoverConfig msg =
  { state : State
  , onHover : State -> Int -> msg
  , onLeave : State -> msg
  }


customConfig
  : { orientation : Orientation
    , symbols : Maybe (SymbolsCustomization msg)
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
        , onClear = options.onClear
        , hoverConfig = options.hoverConfig
        }

    customization =
      { orientation = options.orientation
      , symbols = Maybe.withDefault defaultSymbols options.symbols
      }
  in
  Config { mode = mode, customization = Customization customization }


-- VIEW


viewReadOnly : Rating -> Html msg
viewReadOnly =
  view (Config { mode = ReadOnly, customization = defaultCustomization })


viewCustomReadOnly
  : { orientation : Orientation
    , symbols : SymbolsCustomization msg
    }
  -> Rating
  -> Html msg
viewCustomReadOnly customization =
  view (Config { mode = ReadOnly, customization = Customization customization })


viewDisabled : Rating -> Html msg
viewDisabled =
  view (Config { mode = Disabled, customization = defaultCustomization })


viewCustomDisabled
  : { orientation : Orientation
    , symbols : SymbolsCustomization msg
    }
  -> Rating
  -> Html msg
viewCustomDisabled customization =
  view (Config { mode = Disabled, customization = Customization customization })


viewSimple : (Rating -> msg) -> Rating -> Html msg
viewSimple onChange rating =
  let
    config =
      customConfig
        { orientation = Horizontal
        , symbols = Nothing
        , onChange = onChange
        , onClear = Nothing
        , hoverConfig = Nothing
        }
  in
  view config rating


viewCustomSimple
  : { orientation : Orientation
    , symbols : SymbolsCustomization msg
    , onChange : Rating -> msg
    }
  -> Rating
  -> Html msg
viewCustomSimple options rating =
  let
    config =
      customConfig
        { orientation = options.orientation
        , symbols = Just options.symbols
        , onChange = options.onChange
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
        , symbols = Nothing
        , onChange = onChange
        , onClear = Just onClear
        , hoverConfig = Nothing
        }
  in
  view config rating


viewCustomClearable
  : { orientation : Orientation
    , symbols : SymbolsCustomization msg
    , onChange : Rating -> msg
    , onClear : msg
    }
  -> Rating
  -> Html msg
viewCustomClearable options rating =
  let
    config =
      customConfig
        { orientation = options.orientation
        , symbols = Just options.symbols
        , onChange = options.onChange
        , onClear = Just options.onClear
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
    allOptions =
      case defaultCustomization of
        Customization customization ->
          { orientation = customization.orientation
          , symbols = customization.symbols
          , onChange = options.onChange
          , onClear = options.onClear
          , onHover = options.onHover
          , onLeave = options.onLeave
          }
  in
  viewCustomHoverable allOptions state rating


viewCustomHoverable
  : { orientation : Orientation
    , symbols : SymbolsCustomization msg
    , onChange : Rating -> msg
    , onClear : Maybe msg
    , onHover : State -> Int -> msg
    , onLeave : State -> msg
    }
  -> State
  -> Rating
  -> Html msg
viewCustomHoverable options state rating =
  let
    config =
      customConfig
        { orientation = options.orientation
        , symbols = Just options.symbols
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


view : Config msg -> Rating -> Html msg
view (Config config) rating =
  let
    (orientation, symbolEmpty, symbolFull) =
      case config.customization of
        Customization customization ->
          case customization.symbols of
            Same symbol ->
              (customization.orientation, symbol, symbol)

            Different symbols ->
              (customization.orientation, symbols.symbolEmpty, symbols.symbolFull)
  in
  case config.mode of
    ReadOnly ->
      viewHelper
        { orientation = orientation
        , symbolEmpty = symbolEmpty
        , symbolFull = symbolFull
        , hoverConfig = Nothing
        , blockAttrs = [ class "elm-rater--read-only" ]
        , symbolAttrs = always []
        }
        rating

    Disabled ->
      viewHelper
        { orientation = orientation
        , symbolEmpty = symbolEmpty
        , symbolFull = symbolFull
        , hoverConfig = Nothing
        , blockAttrs = [ class "elm-rater--disabled" ]
        , symbolAttrs = always []
        }
        rating

    Active activeConfig ->
      let
        changeConfig =
          { onChange = activeConfig.onChange
          , onClear = activeConfig.onClear
          }
      in
      viewHelper
        { orientation = orientation
        , symbolEmpty = symbolEmpty
        , symbolFull = symbolFull
        , hoverConfig = activeConfig.hoverConfig
        , blockAttrs = []
        , symbolAttrs =
            \value ->
              List.concat
                [ onClickAttrs changeConfig rating value
                , onHoverAttrs activeConfig.hoverConfig rating value
                ]
        }
        rating


onClickAttrs
  : { onChange : Rating -> msg
    , onClear : Maybe msg
    }
  -> Rating
  -> Int
  -> List (Attribute msg)
onClickAttrs handlers rating value =
  let
    ratio =
      Rating.ratio rating
  in
    case handlers.onClear of
      Nothing ->
        if value == ratio.value then
          []
        else
          [ E.onClick (handlers.onChange <| Rating.rate value rating) ]

      Just onClear ->
        if value == ratio.value then
          [ E.onClick onClear ]
        else
          [ E.onClick (handlers.onChange <| Rating.rate value rating) ]


onHoverAttrs : Maybe (HoverConfig msg) -> Rating -> Int -> List (Attribute msg)
onHoverAttrs hoverConfig rating value =
  let
    ratio =
      Rating.ratio rating
  in
  case hoverConfig of
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
    , hoverConfig : Maybe (HoverConfig msg)
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
        (symbolBuilders symbols maybeState rating)

    symbols =
      { symbolEmpty = options.symbolEmpty
      , symbolFull = options.symbolFull
      }

    maybeState =
      case options.hoverConfig of
        Nothing ->
          Nothing

        Just { state } ->
          Just state

  in
  div blockAttrs [ div [ class "elm-rater__symbols" ] viewSymbols ]


type SymbolBuilder msg
  = Empty (Int -> Html msg)
  | Full (Int -> Html msg)


symbolBuilders
  : { symbolEmpty : Int -> Html msg
    , symbolFull : Int -> Html msg
    }
  -> Maybe State
  -> Rating
  -> List (SymbolBuilder msg)
symbolBuilders { symbolEmpty, symbolFull } maybeState rating =
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


viewSymbol : (Int -> List (Attribute msg)) -> Int -> SymbolBuilder msg -> Html msg
viewSymbol toAttrs index symbolBuilder =
  let
    value =
      index + 1

    (symbolType, toSymbol) =
      case symbolBuilder of
        Empty builder ->
          ("elm-rater__symbol--empty", builder)

        Full builder ->
          ("elm-rater__symbol--full", builder)

    symbolAttrs =
      List.concat
        [ [ class "elm-rater__symbol", class symbolType ]
        , toAttrs value
        ]
  in
  div symbolAttrs [ toSymbol value ]
