module Main exposing (main)

import Browser
import Html exposing (Html)

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
  { rater : Rater.State }


init : Model
init =
  { rater = Rater.initial 3 }


-- UPDATE


type Msg
  = NewRaterMsg Rater.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewRaterMsg raterMsg ->
      { model | rater = Rater.update raterMsg model.rater }


-- VIEW


view : Model -> Html Msg
view { rater } =
  Html.map NewRaterMsg (Rater.view 5 rater)
