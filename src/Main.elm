module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)
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
  { rater1 : Rater.State
  , rating1 : Rating

  , rater2 : Rater.State
  , rating2 : Rating

  , rater3 : Rater.State
  , rating3 : Rating

  , rater4 : Rater.State
  , rating4 : Rating

  , rater5 : Rater.State
  , rating5 : Rating
  , rater5MaybeTransientValue : Maybe Int
  }


init : Model
init =
  { rater1 = Rater.init
  , rating1 = Rating.outOf5 3

  , rater2 = Rater.init
  , rating2 = Rating.outOf 25 20

  , rater3 = Rater.init
  , rating3 = Rating.outOf5 1

  , rater4 = Rater.init
  , rating4 = Rating.outOf5 3

  , rater5 = Rater.init
  , rating5 = Rating.outOf5 2
  , rater5MaybeTransientValue = Nothing
  }


-- UPDATE


type Msg
  = ChangedRating1 Rater.State Rating

  | ChangedRating2 Rater.State Rating

  | ChangedRating3 Rater.State Rating
  | ClickedClear3

  | ChangedRating4 Rater.State Rating
  | HoveredOverRater4 Rater.State Int
  | LeftRater4 Rater.State

  | ChangedRating5 Rater.State Rating
  | HoveredOverRater5 Rater.State Int
  | LeftRater5 Rater.State


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangedRating1 state newRating ->
      { model | rater1 = state, rating1 = newRating }

    ChangedRating2 state newRating ->
      { model | rater2 = state, rating2 = newRating }

    ChangedRating3 state newRating ->
      { model | rater3 = state, rating3 = newRating }

    ClickedClear3 ->
      { model | rating3 = Rating.outOf5 0 }

    ChangedRating4 state newRating ->
      { model | rater4 = state, rating4 = newRating }

    HoveredOverRater4 state _ ->
      { model | rater4 = state }

    LeftRater4 state ->
      { model | rater4 = state }

    ChangedRating5 state newRating ->
      { model | rater5 = state, rating5 = newRating }

    HoveredOverRater5 state value ->
      { model | rater5 = state, rater5MaybeTransientValue = Just value }

    LeftRater5 state ->
      { model | rater5 = state, rater5MaybeTransientValue = Nothing }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm Rater Examples" ]

    , h2 [] [ text "A 5 star rater" ]
    , Rater.view ChangedRating1 Nothing Nothing True model.rater1 model.rating1

    , h2 [] [ text "You can have any number of stars, for e.g. 25" ]
    , Rater.view ChangedRating2 Nothing Nothing True model.rater2 model.rating2

    , h2 [] [ text "You can disable clearing" ]
    , p []
        [ text <| String.join " "
            [ "Usually you can clear the rater by clicking on its current"
            , "rating. Try it on the above raters. However, on this rater"
            , "clearing has been disabled."
            ]
        ]
    , Rater.view ChangedRating3 Nothing Nothing False model.rater3 model.rating3
    , p [] [ text "Why not clear the rating using a button instead?" ]
    , button [ E.onClick ClickedClear3 ] [ text "Clear" ]

    , h2 [] [ text "You can enable hovering" ]
    , Rater.view ChangedRating4 (Just HoveredOverRater4) (Just LeftRater4) True model.rater4 model.rating4

    , h2 [] [ text "Customize onHover and onLeave" ]
    , p []
        [ text <|
            case model.rater5MaybeTransientValue of
              Nothing ->
                "Hover over the rater to see this message change."

              Just transientValue ->
                "You are currently over: " ++ String.fromInt transientValue
        ]
    , Rater.view ChangedRating5 (Just HoveredOverRater5) (Just LeftRater5) True model.rater5 model.rating5
    ]
