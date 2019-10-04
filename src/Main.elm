module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, p, text)

import Rater


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
  , rater2 : Rater.State
  , rater3 : Rater.State
  }


init : Model
init =
  { rater1 = Rater.initial 3
  , rater2 = Rater.initial 20
  , rater3 = Rater.initial 1
  }


-- UPDATE


type Msg
  = NewRaterMsg1 Rater.Msg
  | NewRaterMsg2 Rater.Msg
  | NewRaterMsg3 Rater.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewRaterMsg1 raterMsg ->
      { model | rater1 = Rater.update True raterMsg model.rater1 }

    NewRaterMsg2 raterMsg ->
      { model | rater2 = Rater.update True raterMsg model.rater2 }

    NewRaterMsg3 raterMsg ->
      { model | rater3 = Rater.update False raterMsg model.rater3 }


-- VIEW


view : Model -> Html Msg
view { rater1, rater2, rater3 } =
  div []
    [ h1 [] [ text "Elm Rater Examples" ]

    , h2 [] [ text "A 5 star rater" ]
    , Html.map NewRaterMsg1 (Rater.view 5 rater1)

    , h2 [] [ text "You can have any number of stars, for e.g. 25" ]
    , Html.map NewRaterMsg2 (Rater.view 25 rater2)

    , h2 [] [ text "You can disable clearing" ]
    , p []
        [ text <| String.join " "
            [ "Usually you can clear the rater by clicking on its current"
            , "rating. Try it on the above raters. However, on this rater"
            , "clearing has been disabled."
            ]
        ]
    , Html.map NewRaterMsg3 (Rater.view 5 rater3)
    ]
