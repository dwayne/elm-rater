module Rater exposing
  ( State
  , initial

  , UpdateConfig
  , defaultUpdateConfig

  , Msg
  , update, updateCustom

  , ViewConfig
  , defaultViewConfig

  , defaultAttrs, defaultReadOnlyAttrs, defaultDisabledAttrs
  , defaultWrapperAttrs, defaultReadOnlyWrapperAttrs, defaultDisabledWrapperAttrs

  , view, viewCustom
  , viewReadOnly, viewReadOnlyCustom
  , viewDisabled, viewDisabledCustom
  )

import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes as Attributes exposing (class, style)
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
  { attrs : Maybe (List (Attribute Never))
  , wrapperAttrs : Maybe (List (Attribute Never))
  , selected : Int -> Html Never
  , unselected : Int -> Html Never
  }


defaultViewConfig : ViewConfig
defaultViewConfig =
  { attrs = Nothing
  , wrapperAttrs = Nothing
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


defaultAttrs : List (Attribute Never)
defaultAttrs =
  [ style "display" "inline-block"
  , style "cursor" "pointer"
  ]


defaultReadOnlyAttrs : List (Attribute Never)
defaultReadOnlyAttrs =
  [ style "display" "inline-block"
  , style "cursor" "default"
  ]


defaultDisabledAttrs : List (Attribute Never)
defaultDisabledAttrs =
  [ style "display" "inline-block"
  , style "cursor" "default"
  , style "opacity" "0.5"
  ]


defaultWrapperAttrs : List (Attribute Never)
defaultWrapperAttrs =
  [ style "display" "inline-block" ]


defaultReadOnlyWrapperAttrs : List (Attribute Never)
defaultReadOnlyWrapperAttrs =
  defaultWrapperAttrs


defaultDisabledWrapperAttrs : List (Attribute Never)
defaultDisabledWrapperAttrs =
  defaultWrapperAttrs


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
viewRater { attrs, wrapperAttrs, selected, unselected } total value =
  let
    attrsToUse =
      Maybe.withDefault defaultAttrs attrs

    wrapperAttrsToUse =
      Maybe.withDefault defaultWrapperAttrs wrapperAttrs

    events =
      [ Events.onMouseOut MouseOut ]
  in
    div ((List.map mapAttributeNeverToMsg attrsToUse) ++ events) <|
      viewStars 1 value (viewStar wrapperAttrsToUse selected) ++ viewStars (value + 1) total (viewStar wrapperAttrsToUse unselected)


viewStar : List (Attribute Never) -> (Int -> Html Never) -> Int -> Html Msg
viewStar attrs star value =
  let
    events =
      [ Events.onMouseOver (MouseOver value)
      , Events.onClick (Clicked value)
      ]
  in
    div
      ((List.map mapAttributeNeverToMsg attrs) ++ events)
      [ mapHtmlNeverToMsg (star value) ]


viewReadOnly : Rating -> Html Never
viewReadOnly =
  viewReadOnlyCustom defaultViewConfig


viewReadOnlyCustom : ViewConfig -> Rating -> Html Never
viewReadOnlyCustom { attrs, wrapperAttrs, selected, unselected } rating =
  let
    attrsToUse =
      Maybe.withDefault defaultReadOnlyAttrs attrs

    wrapperAttrsToUse =
      Maybe.withDefault defaultReadOnlyWrapperAttrs wrapperAttrs

    value =
      Rating.value rating

    total =
      Rating.total rating
  in
    div attrsToUse <|
      viewStars 1 value (viewReadOnlyStar wrapperAttrsToUse selected) ++ viewStars (value + 1) total (viewReadOnlyStar wrapperAttrsToUse unselected)


viewReadOnlyStar : List (Attribute msg) -> (Int -> Html msg) -> Int -> Html msg
viewReadOnlyStar attrs star value =
  div attrs [ star value ]


viewDisabled : Rating -> Html Never
viewDisabled =
  viewDisabledCustom defaultViewConfig


viewDisabledCustom : ViewConfig -> Rating -> Html Never
viewDisabledCustom { attrs, wrapperAttrs, selected, unselected } rating =
  let
    attrsToUse =
      Maybe.withDefault defaultDisabledAttrs attrs

    wrapperAttrsToUse =
      Maybe.withDefault defaultDisabledWrapperAttrs wrapperAttrs

    value =
      Rating.value rating

    total =
      Rating.total rating
  in
    div attrsToUse <|
      viewStars 1 value (viewDisabledStar wrapperAttrsToUse selected) ++ viewStars (value + 1) total (viewDisabledStar wrapperAttrsToUse unselected)


viewDisabledStar : List (Attribute msg) -> (Int -> Html msg) -> Int -> Html msg
viewDisabledStar attrs star value =
  div attrs [ star value ]


viewStars : Int -> Int -> (Int -> Html msg) -> List (Html msg)
viewStars low high starBuilder =
  List.range low high
    |> List.map starBuilder


-- HELPERS


mapAttributeNeverToMsg : Attribute Never -> Attribute Msg
mapAttributeNeverToMsg =
  Attributes.map (always NoOp)


mapHtmlNeverToMsg : Html Never -> Html Msg
mapHtmlNeverToMsg =
  Html.map (always NoOp)


maybeApply : Maybe (a -> b) -> a -> Maybe b
maybeApply maybeF x =
  case maybeF of
    Nothing ->
      Nothing

    Just f ->
      Just (f x)
