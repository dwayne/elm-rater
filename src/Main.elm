module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, text)
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
  }


init : Model
init =
  { rating1 = Rating.outOf5 3
  , rating2 = Rating.outOf 25 20
  }


-- UPDATE


type Msg
  = ChangedRating1 Rating
  | ChangedRating2 Rating


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangedRating1 newRating ->
      { model | rating1 = newRating }

    ChangedRating2 newRating ->
      { model | rating2 = newRating }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Elm Rater Examples" ]

    , h2 [] [ text "A 5 star rater" ]
    , Rater.view ChangedRating1 model.rating1

    , h2 [] [ text "You can have any number of stars, for e.g. 25" ]
    , Rater.view ChangedRating2 model.rating2
    ]
