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
  { rating1 : Rating
  , rating2 : Rating
  , rating3 : Rating
  }


init : Model
init =
  { rating1 = Rating.outOf5 3
  , rating2 = Rating.outOf 25 20
  , rating3 = Rating.outOf5 1
  }


-- UPDATE


type Msg
  = ChangedRating1 Rating
  | HoveredOver1 Int
  | ChangedRating2 Rating
  | ChangedRating3 Rating
  | ClickedClear3


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangedRating1 newRating ->
      { model | rating1 = newRating }

    HoveredOver1 _ ->
      model

    ChangedRating2 newRating ->
      { model | rating2 = newRating }

    ChangedRating3 newRating ->
      { model | rating3 = newRating }

    ClickedClear3 ->
      { model | rating3 = Rating.outOf5 0 }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm Rater Examples" ]

    , h2 [] [ text "A 5 star rater" ]
    , Rater.view ChangedRating1 (Just HoveredOver1) True model.rating1

    , h2 [] [ text "You can have any number of stars, for e.g. 25" ]
    , Rater.view ChangedRating2 Nothing True model.rating2

    , h2 [] [ text "You can disable clearing" ]
    , p []
        [ text <| String.join " "
            [ "Usually you can clear the rater by clicking on its current"
            , "rating. Try it on the above raters. However, on this rater"
            , "clearing has been disabled."
            ]
        ]
    , Rater.view ChangedRating3 Nothing False model.rating3
    , p [] [ text "Why not clear the rating using a button instead?" ]
    , button [ E.onClick ClickedClear3 ] [ text "Clear" ]
    ]
