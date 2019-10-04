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

  , rater4 : Rater.State
  , rater4TransientRating : Maybe Int
  }


init : Model
init =
  { rater1 = Rater.initial 3
  , rater2 = Rater.initial 20
  , rater3 = Rater.initial 1

  , rater4 = Rater.initial 2
  , rater4TransientRating = Nothing
  }


-- UPDATE


type Msg
  = NewRaterMsg1 Rater.Msg
  | NewRaterMsg2 Rater.Msg
  | NewRaterMsg3 Rater.Msg
  | NewRaterMsg4 Rater.Msg
  | HoveredOverRater4 Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewRaterMsg1 raterMsg ->
      { model
      | rater1 =
          Tuple.first <| Rater.update True Nothing raterMsg model.rater1
      }

    NewRaterMsg2 raterMsg ->
      { model
      | rater2 =
          Tuple.first <| Rater.update True Nothing raterMsg model.rater2
      }

    NewRaterMsg3 raterMsg ->
      { model
      | rater3 =
          Tuple.first <| Rater.update False Nothing raterMsg model.rater3
      }

    NewRaterMsg4 raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.update
            True
            (Just HoveredOverRater4)
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

    HoveredOverRater4 transientRating ->
      { model | rater4TransientRating = Just transientRating }


-- VIEW


view : Model -> Html Msg
view { rater1, rater2, rater3, rater4, rater4TransientRating } =
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

    , h2 [] [ text "Customize onHover" ]
    , p []
        [ text <|
            case rater4TransientRating of
              Nothing ->
                "Hover over the rater to see this message change."

              Just transientRating ->
                "You are currently over: " ++ String.fromInt transientRating
        ]
    , Html.map NewRaterMsg4 (Rater.view 5 rater4)
    ]
