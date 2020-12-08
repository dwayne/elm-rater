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

  , rater4 : Rater.State
  , rating4 : Rating

  , rater5 : Rater.State
  , rating5 : Rating
  , rater5MaybeTransientValue : Maybe Int

  , rating6 : Rating

  , rater7 : Rater.State
  , rating7 : Rating

  , rating8 : Rating

  , rater9 : Rater.State
  , rating9 : Rating
  , rater9MaybeTransientValue : Maybe Int
  }


init : Model
init =
  { rating1 = Rating.outOf5 3
  , rating2 = Rating.outOf 25 20
  , rating3 = Rating.outOf5 1

  , rater4 = Rater.init
  , rating4 = Rating.outOf5 3

  , rater5 = Rater.init
  , rating5 = Rating.outOf5 2
  , rater5MaybeTransientValue = Nothing

  , rating6 = Rating.outOf5 3

  , rater7 = Rater.init
  , rating7 = Rating.outOf5 1

  , rating8 = Rating.outOf 10 5

  , rater9 = Rater.init
  , rating9 = Rating.outOf 10 7
  , rater9MaybeTransientValue = Nothing
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
        [ Rater.view
            (Rater.customConfig
              { orientation = Rater.Horizontal
              , symbolEmpty = Just <| always (text "_")
              , symbolFull = Just <| text << String.fromInt
              , onChange = ChangedRating7
              , onClear = Nothing
              , hoverConfig =
                  Just
                    { state = model.rater7
                    , onHover = HoveredOverRater7
                    , onLeave = LeftRater7
                    }
              })
            model.rating7
        ]
    , p []
        [ Rater.view
            (Rater.customConfig
              { orientation = Rater.Horizontal
              , symbolEmpty =
                  Just <|
                    \value ->
                      span
                        [ A.class "elm-rater__heart"
                        , A.title (String.fromInt value)
                        ]
                        [ text "\u{2764}" ]
              , symbolFull =
                  Just <|
                    \value ->
                      span
                        [ A.class "elm-rater__heart"
                        , A.title (String.fromInt value)
                        ]
                        [ text "\u{2764}" ]
              , onChange = ChangedRating7
              , onClear = Nothing
              , hoverConfig =
                  Just
                    { state = model.rater7
                    , onHover = HoveredOverRater7
                    , onLeave = LeftRater7
                    }
              })
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
        [ Rater.view
            (Rater.customConfig
              { orientation = Rater.Horizontal
              , symbolEmpty =
                  Just <| always (div [ A.class "rater9__star" ] [])
              , symbolFull =
                  Just <| always (div [ A.class "rater9__star" ] [])
              , onChange = ChangedRating9
              , onClear = Nothing
              , hoverConfig =
                  Just
                    { state = model.rater9
                    , onHover = HoveredOverRater9
                    , onLeave = LeftRater9
                    }
              })
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
    ]
