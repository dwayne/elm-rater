module Main exposing (main)


import Browser
import Html exposing (Html, a, button, div, h1, h2, h3, input, p, span, text)
import Html.Attributes as A
import Html.Events as E
import Rater
import Rater.Rating as Rating exposing (Rating)


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }


-- MODEL


type alias Model =
  { rating1 : Rating
  , rating2 : Rating
  , rating3 : Rating

  , rating4 : Rating
  , rater4 : Rater.State

  , rating5 : Rating
  , rater5 : Rater.State
  , rater5MaybeTransientValue : Maybe Int

  , rating6 : Rating

  , rating7 : Rating
  , rater7 : Rater.State

  , rating8 : Rating

  , rating9 : Rating
  , rater9 : Rater.State
  , rater9MaybeTransientValue : Maybe Int

  , rating10 : Rating
  , rater10 : Rater.State
  , rater10MaybeTransientValue : Maybe Int

  , rating11 : Rating
  , rater11 : Rater.State

  , rating12 : Rating
  , rater12 : Rater.State

  , rating13 : Rating
  , rater13 : Rater.State
  , rater13MaybeTransientValue : Maybe Int
  }


init : Model
init =
  { rating1 = Rating.outOf5 3
  , rating2 = Rating.outOf 25 20
  , rating3 = Rating.outOf5 1

  , rating4 = Rating.outOf5 3
  , rater4 = Rater.init

  , rating5 = Rating.outOf5 2
  , rater5 = Rater.init
  , rater5MaybeTransientValue = Nothing

  , rating6 = Rating.outOf5 3

  , rating7 = Rating.outOf5 1
  , rater7 = Rater.init

  , rating8 = Rating.outOf 10 5

  , rating9 = Rating.outOf 10 7
  , rater9 = Rater.init
  , rater9MaybeTransientValue = Nothing

  , rating10 = Rating.outOf 4 2
  , rater10 = Rater.init
  , rater10MaybeTransientValue = Nothing

  , rating11 = Rating.outOf5 0
  , rater11 = Rater.init

  , rating12 = Rating.outOf 6 1
  , rater12 = Rater.init

  , rating13 = Rating.outOf5 3
  , rater13 = Rater.init
  , rater13MaybeTransientValue = Nothing
  }


-- UPDATE


type Msg
  = ChangedRating1 Rating
  | ChangedRating2 Rating

  | ChangedRating3 Rating
  | ClearedRater3
  | ClickedClear3

  | ChangedRating4 Rating
  | ClearedRater4
  | HoveredOverRater4 Rater.State Int
  | LeftRater4 Rater.State

  | ChangedRating5 Rating
  | HoveredOverRater5 Rater.State Int
  | LeftRater5 Rater.State

  | ChangedRating7 Rating
  | HoveredOverRater7 Rater.State Int
  | LeftRater7 Rater.State

  | ChangedRating8 Rating
  | EnteredInput8 String

  | ChangedRating9 Rating
  | HoveredOverRater9 Rater.State Int
  | LeftRater9 Rater.State

  | ChangedRating10 Rating
  | HoveredOverRater10 Rater.State Int
  | LeftRater10 Rater.State

  | ChangedRating11 Rating
  | ClearedRater11
  | HoveredOverRater11 Rater.State Int
  | LeftRater11 Rater.State

  | ChangedRating12 Rating
  | ClearedRater12
  | HoveredOverRater12 Rater.State Int
  | LeftRater12 Rater.State

  | ChangedRating13 Rating
  | HoveredOverRater13 Rater.State Int
  | LeftRater13 Rater.State


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangedRating1 newRating ->
      { model | rating1 = newRating }

    ChangedRating2 newRating ->
      { model | rating2 = newRating }

    ChangedRating3 newRating ->
      { model | rating3 = newRating }

    ClearedRater3 ->
      { model | rating3 = Rating.zero model.rating3 }

    ClickedClear3 ->
      { model | rating3 = Rating.zero model.rating3 }

    ChangedRating4 newRating ->
      { model | rating4 = newRating }

    ClearedRater4 ->
      { model | rating4 = Rating.zero model.rating4 }

    HoveredOverRater4 state _ ->
      { model | rater4 = state }

    LeftRater4 state ->
      { model | rater4 = state }

    ChangedRating5 newRating ->
      { model | rating5 = newRating }

    HoveredOverRater5 state value ->
      { model | rater5 = state, rater5MaybeTransientValue = Just value }

    LeftRater5 state ->
      { model | rater5 = state, rater5MaybeTransientValue = Nothing }

    ChangedRating7 newRating ->
      { model | rating7 = newRating }

    HoveredOverRater7 state _ ->
      { model | rater7 = state }

    LeftRater7 state ->
      { model | rater7 = state }

    ChangedRating8 newRating ->
      { model | rating8 = newRating }

    EnteredInput8 str ->
      case String.toInt str of
        Nothing ->
          model

        Just value ->
          { model | rating8 = Rating.rate value model.rating8 }

    ChangedRating9 newRating ->
      { model | rating9 = newRating }

    HoveredOverRater9 state value ->
      { model | rater9 = state, rater9MaybeTransientValue = Just value }

    LeftRater9 state ->
      { model | rater9 = state, rater9MaybeTransientValue = Nothing }

    ChangedRating10 newRating ->
      { model | rating10 = newRating }

    HoveredOverRater10 state value ->
      { model | rater10 = state, rater10MaybeTransientValue = Just value }

    LeftRater10 state ->
      { model | rater10 = state, rater10MaybeTransientValue = Nothing }

    ChangedRating11 newRating ->
      { model | rating11 = newRating }

    ClearedRater11 ->
      { model | rating11 = Rating.zero model.rating11 }

    HoveredOverRater11 state _ ->
      { model | rater11 = state }

    LeftRater11 state ->
      { model | rater11 = state }

    ChangedRating12 newRating ->
      { model | rating12 = newRating }

    ClearedRater12 ->
      { model | rating12 = Rating.zero model.rating12 }

    HoveredOverRater12 state _ ->
      { model | rater12 = state }

    LeftRater12 state ->
      { model | rater12 = state }

    ChangedRating13 newRating ->
      { model | rating13 = newRating }

    HoveredOverRater13 state value ->
      { model | rater13 = state, rater13MaybeTransientValue = Just value }

    LeftRater13 state ->
      { model | rater13 = state, rater13MaybeTransientValue = Nothing }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm Rater Examples" ]

    , h2 [] [ text "A 5 star rater" ]
    , Rater.viewSimple ChangedRating1 model.rating1

    , h2 [] [ text "You can have any number of stars, for e.g. 25" ]
    , Rater.viewSimple ChangedRating2 model.rating2

    , h2 [] [ text "You can enable clearing" ]
    , Rater.viewClearable ChangedRating3 ClearedRater3 model.rating3
    , p []
        [ text <| String.join " "
            [ "By default you have to clear the rater by clicking on its"
            , "current rating. However, you don't have to."
            ]
        ]
    , p [] [ text "Why not clear the rater using a button instead?" ]
    , button [ E.onClick ClickedClear3 ] [ text "Clear" ]

    , h2 [] [ text "You can enable hovering" ]
    , Rater.viewHoverable
        { onChange = ChangedRating4
        , onClear = Just ClearedRater4
        , onHover = HoveredOverRater4
        , onLeave = LeftRater4
        }
        model.rater4
        model.rating4

    , h2 [] [ text "Customize onHover and onLeave" ]
    , p []
        [ text <|
            case model.rater5MaybeTransientValue of
              Nothing ->
                "Hover over the rater to see this message change."

              Just transientValue ->
                "You are currently over: " ++ String.fromInt transientValue
        ]
    , Rater.viewHoverable
        { onChange = ChangedRating5
        , onClear = Nothing
        , onHover = HoveredOverRater5
        , onLeave = LeftRater5
        }
        model.rater5
        model.rating5

    , h2 [] [ text "Read only" ]
    , Rater.viewReadOnly model.rating6

    , h2 [] [ text "Disabled" ]
    , Rater.viewDisabled model.rating6

    , h2 [] [ text "More than stars, customize the HTML" ]
    , p []
        [ Rater.viewCustomHoverable
            { orientation = Rater.horizontal
            , symbols =
                Rater.differentSymbols
                  { symbolEmpty = always (text "_")
                  , symbolFull = text << String.fromInt
                  }
            , onChange = ChangedRating7
            , onClear = Nothing
            , onHover = HoveredOverRater7
            , onLeave = LeftRater7
            }
            model.rater7
            model.rating7
        ]
    , p []
        [ Rater.viewCustomHoverable
            { orientation = Rater.horizontal
            , symbols =
                Rater.sameSymbols <|
                  \value ->
                    span
                      [ A.class "elm-rater__heart"
                      , A.title (String.fromInt value)
                      ]
                      [ text "\u{2764}" ]
            , onChange = ChangedRating7
            , onClear = Nothing
            , onHover = HoveredOverRater7
            , onLeave = LeftRater7
            }
            model.rater7
            model.rating7
        ]

    , h2 [] [ text "You own the rating" ]
    , p []
        [ text <| String.join " "
            [ "Let's give the rating to a numeric input as well and"
            , "control the rating using that."
            ]
        ]
    , p [] [ Rater.viewSimple ChangedRating8 model.rating8 ]
    , p []
        [ text "Use this input field to change the rating: "
        , let
            ratio =
              Rating.ratio model.rating8
          in
          input
            [ A.type_ "number"
            , A.value (String.fromInt ratio.value)
            , E.onInput EnteredInput8
            ]
            []
        ]

    , h2 [] [ text "Even more customizations" ]
    , p []
        [ text "I recreated these examples from "
        , a [ A.href "https://antennaio.github.io/jquery-bar-rating/"
            , A.target "_blank"
            ]
            [ text "jQuery Bar Rating" ]
        , text "."
        ]

    , h3 [] [ text "1/10 Rating" ]
    , div
        [ A.class "rater9" ]
        [ Rater.viewCustomHoverable
            { orientation = Rater.horizontal
            , symbols =
                Rater.sameSymbols <|
                  always (div [ A.class "rater9__symbol" ] [])
            , onChange = ChangedRating9
            , onClear = Nothing
            , onHover = HoveredOverRater9
            , onLeave = LeftRater9
            }
            model.rater9
            model.rating9
        , span
            [ A.class "rater9__value" ]
            [ text <|
                String.fromInt <|
                  case model.rater9MaybeTransientValue of
                    Nothing ->
                      (Rating.ratio model.rating9).value

                    Just transientValue ->
                      transientValue
            ]
        ]

    , h3 [] [ text "Movie Rating" ]
    , div [ A.class "rater10" ]
        [ div
            [ A.class "rater10__items" ]
            [ Rater.viewCustomHoverable
                { orientation = Rater.horizontal
                , symbols =
                    Rater.sameSymbols <|
                      always (div [ A.class "rater10__symbol" ] [])
                , onChange = ChangedRating10
                , onClear = Nothing
                , onHover = HoveredOverRater10
                , onLeave = LeftRater10
                }
                model.rater10
                model.rating10
            , span
                [ A.class "rater10__value" ]
                [ text <|
                    toRater10String <|
                      case model.rater10MaybeTransientValue of
                        Nothing ->
                          (Rating.ratio model.rating10).value

                        Just transientValue ->
                          transientValue
                ]
            ]
        ]

    , h3 [] [ text "Square Rating" ]
    , Rater.viewCustomHoverable
        { orientation = Rater.horizontal
        , symbols =
            Rater.sameSymbols <|
              \value ->
                div
                  [ A.class "rater11__symbol" ]
                  [ div [] [ text (String.fromInt value) ] ]
        , onChange = ChangedRating11
        , onClear = Just ClearedRater11
        , onHover = HoveredOverRater11
        , onLeave = LeftRater11
        }
        model.rater11
        model.rating11

    , h3 [] [ text "Pill Rating" ]
    , Rater.viewCustomHoverable
        { orientation = Rater.horizontal
        , symbols =
            Rater.sameSymbols <|
              \value ->
                div
                  [ A.class "rater12__symbol" ]
                  [ text (toRater12String value) ]
        , onChange = ChangedRating12
        , onClear = Just ClearedRater12
        , onHover = HoveredOverRater12
        , onLeave = LeftRater12
        }
        model.rater12
        model.rating12

    , h3 [] [ text "Reversed Rating" ]
    , div
        [ A.class "rater13" ]
        [ Rater.viewCustomHoverable
            { orientation = Rater.horizontalReverse
            , symbols =
                Rater.sameSymbols <|
                  always (div [ A.class "rater13__symbol" ] [])
            , onChange = ChangedRating13
            , onClear = Nothing
            , onHover = HoveredOverRater13
            , onLeave = LeftRater13
            }
            model.rater13
            model.rating13
        , span
            [ A.class "rater13__value" ]
            [ text <|
                toRater13String <|
                  case model.rater13MaybeTransientValue of
                    Nothing ->
                      (Rating.ratio model.rating13).value

                    Just transientValue ->
                      transientValue
            ]
        ]
    ]


-- HELPERS


toRater10String : Int -> String
toRater10String value =
  case value of
    1 ->
      "Bad"

    2 ->
      "Mediocre"

    3 ->
      "Good"

    4 ->
      "Awesome"

    _ ->
      ""


toRater12String : Int -> String
toRater12String value =
  case value of
    1 ->
      "A"

    2 ->
      "B"

    3 ->
      "C"

    4 ->
      "D"

    5 ->
      "E"

    6 ->
      "F"

    _ ->
      ""


toRater13String : Int -> String
toRater13String value =
  case value of
    1 ->
      "Strongly Disagree"

    2 ->
      "Disagree"

    3 ->
      "Neither Agree nor Disagree"

    4 ->
      "Agree"

    5 ->
      "Strongly Agree"

    _ ->
      ""
