module OldMain exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, h2, h3, input, p, span, text)
import Html.Attributes as A
import Html.Events as E

import Rater exposing (defaultUpdateConfig, defaultViewConfig)
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
  , rater1 : Rater.State

  , rating2 : Rating
  , rater2 : Rater.State

  , rating3 : Rating
  , rater3 : Rater.State

  , rating4 : Rating
  , rater4 : Rater.State
  , rater4MaybeTransientAmount : Maybe Int

  , rating5 : Rating

  , rating6 : Rating
  , rater6 : Rater.State

  , rating7 : Rating
  , rater7 : Rater.State

  , rating8 : Rating
  , rater8 : Rater.State

  , rating9 : Rating
  , rater9 : Rater.State
  , rater9MaybeTransientAmount : Maybe Int

  , rating10 : Rating
  , rater10 : Rater.State
  , rater10MaybeTransientAmount : Maybe Int

  , rating11 : Rating
  , rater11 : Rater.State

  , rating12 : Rating
  , rater12 : Rater.State

  , rating13 : Rating
  , rater13 : Rater.State
  , rater13MaybeTransientAmount : Maybe Int

  , rating14 : Rating
  , rater14 : Rater.State
  }


init : Model
init =
  { rating1 = Rating.rate 3 5
  , rater1 = Rater.init

  , rating2 = Rating.rate 20 25
  , rater2 = Rater.init

  , rating3 = Rating.rate 1 5
  , rater3 = Rater.init

  , rating4 = Rating.rate 2 5
  , rater4 = Rater.init
  , rater4MaybeTransientAmount = Nothing

  , rating5 = Rating.rate 3 5

  , rating6 = Rating.rate 0 10
  , rater6 = Rater.init

  , rating7 = Rating.rate 1 5
  , rater7 = Rater.init

  , rating8 = Rating.rate 5 10
  , rater8 = Rater.init

  , rating9 = Rating.rate 7 10
  , rater9 = Rater.init
  , rater9MaybeTransientAmount = Nothing

  , rating10 = Rating.rate 2 4
  , rater10 = Rater.init
  , rater10MaybeTransientAmount = Nothing

  , rating11 = Rating.rate 0 5
  , rater11 = Rater.init

  , rating12 = Rating.rate 1 6
  , rater12 = Rater.init

  , rating13 = Rating.rate 3 5
  , rater13 = Rater.init
  , rater13MaybeTransientAmount = Nothing

  , rating14 = Rating.rate 1 10
  , rater14 = Rater.init
  }


-- UPDATE


type Msg
  = NewRater1Msg Rater.Msg
  | NewRater2Msg Rater.Msg
  | NewRater3Msg Rater.Msg
  | NewRater4Msg Rater.Msg
  | NewRater6Msg Rater.Msg
  | NewRater7Msg Rater.Msg
  | NewRater8Msg Rater.Msg
  | NewRater9Msg Rater.Msg
  | NewRater10Msg Rater.Msg
  | NewRater11Msg Rater.Msg
  | NewRater12Msg Rater.Msg
  | NewRater13Msg Rater.Msg
  | NewRater14Msg Rater.Msg
  | ChangedRater1 Int
  | ChangedRater2 Int
  | ChangedRater3 Int
  | ChangedRater4 Int
  | ChangedRater6 Int
  | ChangedRater7 Int
  | ChangedRater8 Int
  | ChangedRater9 Int
  | ChangedRater10 Int
  | ChangedRater11 Int
  | ChangedRater12 Int
  | ChangedRater13 Int
  | ChangedRater14 Int
  | ClickedClear3
  | HoveredOverRater4 Int
  | LeftRater4
  | NewInput8 String
  | HoveredOverRater9 Int
  | LeftRater9
  | HoveredOverRater10 Int
  | LeftRater10
  | HoveredOverRater13 Int
  | LeftRater13
  | NoOp


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewRater1Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater1 model.rating1 raterMsg model.rater1

        newModel =
          { model | rater1 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater2Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater2 model.rating2 raterMsg model.rater2

        newModel =
          { model | rater2 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater3Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | clearable = False
            , onChange = Just ChangedRater3
            }
            model.rating3
            raterMsg
            model.rater3

        newModel =
          { model | rater3 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater4Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | onChange = Just ChangedRater4
            , onHover = Just HoveredOverRater4
            , onLeave = Just LeftRater4
            }
            model.rating4
            raterMsg
            model.rater4

        newModel =
          { model | rater4 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater6Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig | onChange = Just ChangedRater6 }
            model.rating6
            raterMsg
            model.rater6

        newModel =
          { model | rater6 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater7Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater7 model.rating7 raterMsg model.rater7

        newModel =
          { model | rater7 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater8Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater8 model.rating8 raterMsg model.rater8

        newModel =
          { model | rater8 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater9Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | onChange = Just ChangedRater9
            , onHover = Just HoveredOverRater9
            , onLeave = Just LeftRater9
            }
            model.rating9
            raterMsg
            model.rater9

        newModel =
          { model | rater9 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater10Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | clearable = False
            , onChange = Just ChangedRater10
            , onHover = Just HoveredOverRater10
            , onLeave = Just LeftRater10
            }
            model.rating10
            raterMsg
            model.rater10

        newModel =
          { model | rater10 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater11Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater11 model.rating11 raterMsg model.rater11

        newModel =
          { model | rater11 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater12Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update ChangedRater12 model.rating12 raterMsg model.rater12

        newModel =
          { model | rater12 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater13Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | clearable = False
            , onChange = Just ChangedRater13
            , onHover = Just HoveredOverRater13
            , onLeave = Just LeftRater13
            }
            model.rating13
            raterMsg
            model.rater13

        newModel =
          { model | rater13 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    NewRater14Msg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | clearable = False
            , hoverable = False
            , onChange = Just ChangedRater14
            }
            model.rating14
            raterMsg
            model.rater14

        newModel =
          { model | rater14 = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    ChangedRater1 newAmount ->
      { model | rating1 = Rating.change newAmount model.rating1 }

    ChangedRater2 newAmount ->
      { model | rating2 = Rating.change newAmount model.rating2 }

    ChangedRater3 newAmount ->
      { model | rating3 = Rating.change newAmount model.rating3 }

    ChangedRater4 newAmount ->
      { model | rating4 = Rating.change newAmount model.rating4 }

    ChangedRater6 newAmount ->
      { model | rating6 = Rating.change newAmount model.rating6 }

    ChangedRater7 newAmount ->
      { model | rating7 = Rating.change newAmount model.rating7 }

    ChangedRater8 newAmount ->
      { model | rating8 = Rating.change newAmount model.rating8 }

    ChangedRater9 newAmount ->
      { model | rating9 = Rating.change newAmount model.rating9 }

    ChangedRater10 newAmount ->
      { model | rating10 = Rating.change newAmount model.rating10 }

    ChangedRater11 newAmount ->
      { model | rating11 = Rating.change newAmount model.rating11 }

    ChangedRater12 newAmount ->
      { model | rating12 = Rating.change newAmount model.rating12 }

    ChangedRater13 newAmount ->
      { model | rating13 = Rating.change newAmount model.rating13 }

    ChangedRater14 newAmount ->
      { model | rating14 = Rating.change newAmount model.rating14 }

    ClickedClear3 ->
      { model | rating3 = Rating.change 0 model.rating3 }

    HoveredOverRater4 transientAmount ->
      { model | rater4MaybeTransientAmount = Just transientAmount }

    LeftRater4 ->
      { model | rater4MaybeTransientAmount = Nothing }

    NewInput8 newAmountString ->
      case String.toInt newAmountString of
        Nothing ->
          model

        Just newAmount ->
          { model | rating8 = Rating.change newAmount model.rating8 }

    HoveredOverRater9 transientAmount ->
      { model | rater9MaybeTransientAmount = Just transientAmount }

    LeftRater9 ->
      { model | rater9MaybeTransientAmount = Nothing }

    HoveredOverRater10 transientAmount ->
      { model | rater10MaybeTransientAmount = Just transientAmount }

    LeftRater10 ->
      { model | rater10MaybeTransientAmount = Nothing }

    HoveredOverRater13 transientAmount ->
      { model | rater13MaybeTransientAmount = Just transientAmount }

    LeftRater13 ->
      { model | rater13MaybeTransientAmount = Nothing }

    NoOp ->
      model


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm Rater Examples" ]

    , h2 [] [ text "A 5 star rater" ]
    , Html.map NewRater1Msg (Rater.view model.rating1 model.rater1)

    , h2 [] [ text "You can have any number of stars, for e.g. 25" ]
    , Html.map NewRater2Msg (Rater.view model.rating2 model.rater2)

    , h2 [] [ text "You can disable clearing" ]
    , p []
        [ text <| String.join " "
            [ "Usually you can clear the rater by clicking on its current"
            , "rating. Try it on the above raters. However, on this rater"
            , "clearing has been disabled."
            ]
        ]
    , Html.map NewRater3Msg (Rater.view model.rating3 model.rater3)
    , p [] [ text "Why not clear the rating using a button instead?" ]
    , button [ E.onClick ClickedClear3 ] [ text "Clear" ]

    , h2 [] [ text "Customize onHover and onLeave" ]
    , p []
        [ text <|
            case model.rater4MaybeTransientAmount of
              Nothing ->
                "Hover over the rater to see this message change."

              Just transientAmount ->
                "You are currently over: " ++ String.fromInt transientAmount
        ]
    , Html.map NewRater4Msg (Rater.view model.rating4 model.rater4)

    , h2 [] [ text "Read only" ]
    , Rater.viewReadOnly NoOp model.rating5

    , h2 [] [ text "Disabled" ]
    , Rater.viewDisabled NoOp model.rating5

    , h2 [] [ text "Customize onChange" ]
    , p []
        [ text <|
            let
              amount =
                Rating.amount model.rating6
            in
              if amount == 0 then
                "Rate me."
              else
                "You selected: " ++ String.fromInt amount
        ]
    , Html.map NewRater6Msg (Rater.view model.rating6 model.rater6)

    , h2 [] [ text "More than stars, customize the HTML" ]
    , p []
        [ Html.map NewRater7Msg <|
            Rater.viewCustom
              { defaultViewConfig
              | selected = text << String.fromInt
              , unselected = always (text "_")
              }
              model.rating7
              model.rater7
        ]
    , p []
        [ Html.map NewRater7Msg <|
            Rater.viewCustom
              { defaultViewConfig
              | selected = (
                  \n ->
                    span
                      [ A.style "font-size" "32px"
                      , A.style "color" "red"
                      , A.title (String.fromInt n)
                      ]
                      [ text "\u{2764}" ]
                )
              , unselected = (
                  \n ->
                    span
                      [ A.style "font-size" "32px"
                      , A.style "color" "black"
                      , A.title (String.fromInt n)
                      ]
                      [ text "\u{2764}" ]
                )
              }
              model.rating7
              model.rater7
        ]

    , h2 [] [ text "You own the rating" ]
    , p []
        [ text <| String.join " "
            [ "Let's give the rating to a numeric input as well and"
            , "control the rating using that."
            ]
        ]
    , p [] [ Html.map NewRater8Msg (Rater.view model.rating8 model.rater8) ]
    , p []
        [ text "Use this input field to change the rating: "
        , input
            [ A.type_ "number"
            , A.value (String.fromInt (Rating.amount model.rating8))
            , E.onInput NewInput8
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
        [ A.class "rater9-container" ]
        [ Html.map NewRater9Msg <|
            Rater.viewCustom
              { defaultViewConfig
              | selected = (
                  \_ ->
                    div [ A.class "rater9__star is-selected" ] []
                )
              , unselected = (
                  \_ ->
                    div [ A.class "rater9__star" ] []
              )
              }
              model.rating9
              model.rater9
        , span
            [ A.class "amount9" ]
            [ text <|
                String.fromInt <|
                  case model.rater9MaybeTransientAmount of
                    Nothing ->
                      Rating.amount model.rating9

                    Just transientAmount ->
                      transientAmount
            ]
        ]

    , h3 [] [ text "Movie Rating" ]
    , div [ A.class "rater10-container" ]
        [ div
            [ A.class "rater10-inner-container" ]
            [ Html.map NewRater10Msg <|
                Rater.viewCustom
                  { defaultViewConfig
                  | selected = (
                      \_ ->
                        div [ A.class "rater10__star is-selected" ] []
                    )
                  , unselected = (
                      \_ ->
                        div [ A.class "rater10__star" ] []
                  )
                  }
                  model.rating10
                  model.rater10
            , span
                [ A.class "value10" ]
                [ text <|
                    toRater10Value <|
                      case model.rater10MaybeTransientAmount of
                        Nothing ->
                          Rating.amount model.rating10

                        Just transientAmount ->
                          transientAmount
                ]
            ]
        ]

    , h3 [] [ text "Square Rating" ]
    , Html.map NewRater11Msg <|
        Rater.viewCustom
          { defaultViewConfig
          | selected = (
              \amount ->
                div
                  [ A.class "rater11__star is-selected" ]
                  [ div [] [ text (String.fromInt amount) ] ]
            )
          , unselected = (
              \amount ->
                div
                  [ A.class "rater11__star" ]
                  [ div [] [ text (String.fromInt amount) ] ]
          )
          }
          model.rating11
          model.rater11

    , h3 [] [ text "Pill Rating" ]
    , Html.map NewRater12Msg <|
        Rater.viewCustom
          { defaultViewConfig
          | wrapperAttrs = Just [ A.class "rater12__star-wrapper" ]
          , selected = (
              \amount ->
                div
                  [ A.class "rater12__star is-selected" ]
                  [ text (toRater12Value amount) ]
            )
          , unselected = (
              \amount ->
                div
                  [ A.class "rater12__star" ]
                  [ text (toRater12Value amount) ]
          )
          }
          model.rating12
          model.rater12

    , h3 [] [ text "Reversed Rating" ]
    , div
        [ A.class "rater13-container" ]
        [ Html.map NewRater13Msg <|
            Rater.viewCustom
              { defaultViewConfig
              | attrs = Just [ A.class "rater13" ]
              , selected = (
                  \_ ->
                    div [ A.class "rater13__star is-selected" ] []
                )
              , unselected = (
                  \_ ->
                    div [ A.class "rater13__star" ] []
              )
              }
              model.rating13
              model.rater13
        , span
            [ A.class "value13" ]
            [ text <|
                toRater13Value <|
                  case model.rater13MaybeTransientAmount of
                    Nothing ->
                      Rating.amount model.rating13

                    Just transientAmount ->
                      transientAmount
            ]
        ]

    , h3 [] [ text "Horizontal Rating" ]
    , p [] [ text "Notice that the hover effect is turned off." ]
    , div [ A.class "rater14-container" ]
        [ div
            [ A.class "rater14-inner-container" ]
            [ Html.map NewRater14Msg <|
                Rater.viewCustom
                  { defaultViewConfig
                  | attrs = Just [ A.class "rater14" ]
                  , selected = (
                      \_ ->
                        div [ A.class "rater14__star is-selected" ] []
                    )
                  , unselected = (
                      \_ ->
                        div [ A.class "rater14__star" ] []
                  )
                  }
                  model.rating14
                  model.rater14
            , span
                [ A.class "value14" ]
                [ text (String.fromInt (Rating.amount model.rating14))
                ]
            ]
        ]
    , p [] [ text "Click on a bar to change the rating." ]

    , Html.footer
        []
        [ p [ A.style "font-size" "13px" ]
            [ text "Created by "
            , a [ A.href "https://github.com/dwayne/" ]
                [ text "Dwayne Crooks" ]
            ]
        ]
    ]


-- HELPERS


toRater10Value : Int -> String
toRater10Value amount =
  case amount of
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


toRater12Value : Int -> String
toRater12Value amount =
  case amount of
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


toRater13Value : Int -> String
toRater13Value amount =
  case amount of
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
