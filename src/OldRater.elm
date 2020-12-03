module OldRater exposing
  ( State
  , init

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
import Html.Attributes as A exposing (style)
import Html.Events as E

import Rater.Rating as Rating exposing (Rating)


type State
  = Fixed
  | Transient Int


init : State
init =
  Fixed


type alias UpdateConfig msg =
  { clearable : Bool
  , hoverable : Bool
  , onChange : Maybe (Int -> msg)
  , onHover : Maybe (Int -> msg)
  , onLeave : Maybe msg
  }


defaultUpdateConfig : UpdateConfig msg
defaultUpdateConfig =
  { clearable = True
  , hoverable = True
  , onChange = Nothing
  , onHover = Nothing
  , onLeave = Nothing
  }


type Msg
  = MouseOver Int
  | MouseOut
  | Clicked Int
  | NoOp


update : (Int -> msg) -> Rating -> Msg -> State -> (State, Maybe msg)
update onChange =
  updateCustom { defaultUpdateConfig | onChange = Just onChange }


updateCustom : UpdateConfig msg -> Rating -> Msg -> State -> (State, Maybe msg)
updateCustom config rating msg state =
  case msg of
    MouseOver transientAmount ->
      ( if config.hoverable then
          Transient transientAmount
        else
          Fixed
      , maybeApply config.onHover transientAmount
      )

    MouseOut ->
      ( Fixed
      , config.onLeave
      )

    Clicked newAmount ->
      if newAmount == Rating.amount rating then
        ( Fixed
        , if config.clearable then
            maybeApply config.onChange 0
          else
            Nothing
        )
      else
        ( Fixed
        , maybeApply config.onChange newAmount
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
    amount =
      case state of
        Fixed ->
          Rating.amount rating

        Transient transientAmount ->
          transientAmount

    outOf =
      Rating.outOf rating
  in
    viewRater config amount outOf


viewRater : ViewConfig -> Int -> Int -> Html Msg
viewRater { attrs, wrapperAttrs, selected, unselected } amount outOf =
  let
    attrsToUse =
      Maybe.withDefault defaultAttrs attrs

    wrapperAttrsToUse =
      Maybe.withDefault defaultWrapperAttrs wrapperAttrs

    events =
      [ E.onMouseOut MouseOut ]
  in
    div ((List.map mapAttributeNeverToMsg attrsToUse) ++ events) <|
      viewStars 1 amount (viewStar wrapperAttrsToUse selected) ++ viewStars (amount + 1) outOf (viewStar wrapperAttrsToUse unselected)


viewStar : List (Attribute Never) -> (Int -> Html Never) -> Int -> Html Msg
viewStar attrs star amount =
  let
    events =
      [ E.onMouseOver (MouseOver amount)
      , E.onClick (Clicked amount)
      ]
  in
    div
      ((List.map mapAttributeNeverToMsg attrs) ++ events)
      [ mapHtmlNeverToMsg (star amount) ]


viewReadOnly : msg -> Rating -> Html msg
viewReadOnly =
  viewReadOnlyCustom defaultViewConfig


viewReadOnlyCustom : ViewConfig -> msg -> Rating -> Html msg
viewReadOnlyCustom { attrs, wrapperAttrs, selected, unselected } msg rating =
  let
    attrsToUse =
      Maybe.withDefault defaultReadOnlyAttrs attrs

    wrapperAttrsToUse =
      Maybe.withDefault defaultReadOnlyWrapperAttrs wrapperAttrs

    amount =
      Rating.amount rating

    outOf =
      Rating.outOf rating
  in
    Html.map (always msg) <|
      div attrsToUse <|
        viewStars 1 amount (viewReadOnlyStar wrapperAttrsToUse selected) ++ viewStars (amount + 1) outOf (viewReadOnlyStar wrapperAttrsToUse unselected)


viewReadOnlyStar : List (Attribute msg) -> (Int -> Html msg) -> Int -> Html msg
viewReadOnlyStar attrs star amount =
  div attrs [ star amount ]


viewDisabled : msg -> Rating -> Html msg
viewDisabled =
  viewDisabledCustom defaultViewConfig


viewDisabledCustom : ViewConfig -> msg -> Rating -> Html msg
viewDisabledCustom { attrs, wrapperAttrs, selected, unselected } msg rating =
  let
    attrsToUse =
      Maybe.withDefault defaultDisabledAttrs attrs

    wrapperAttrsToUse =
      Maybe.withDefault defaultDisabledWrapperAttrs wrapperAttrs

    amount =
      Rating.amount rating

    outOf =
      Rating.outOf rating
  in
    Html.map (always msg) <|
      div attrsToUse <|
        viewStars 1 amount (viewDisabledStar wrapperAttrsToUse selected) ++ viewStars (amount + 1) outOf (viewDisabledStar wrapperAttrsToUse unselected)


viewDisabledStar : List (Attribute msg) -> (Int -> Html msg) -> Int -> Html msg
viewDisabledStar attrs star amount =
  div attrs [ star amount ]


viewStars : Int -> Int -> (Int -> Html msg) -> List (Html msg)
viewStars low high starBuilder =
  List.range low high
    |> List.map starBuilder


-- HELPERS


mapAttributeNeverToMsg : Attribute Never -> Attribute Msg
mapAttributeNeverToMsg =
  A.map (always NoOp)


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
