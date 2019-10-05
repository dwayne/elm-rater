module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, p, text)

import Rater exposing (defaultUpdateConfig)


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }


-- MODEL


type alias Model =
  { rater : Rater.State
  , rating : Int
  , maybeTransientRating : Maybe Int
  }


init : Model
init =
  { rater = Rater.initial
  , rating = 3
  , maybeTransientRating = Nothing
  }


-- UPDATE


type Msg
  = NewRaterMsg Rater.Msg
  | Changed Int
  | Hovered Int
  | Left


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewRaterMsg raterMsg ->
      let
        (newState, maybeMsg) =
          Rater.updateCustom
            { defaultUpdateConfig
            | onChange = Just Changed
            , onHover = Just Hovered
            , onLeave = Just Left
            }
            model.rating
            raterMsg
            model.rater

        newModel =
          { model | rater = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel

          Just newMsg ->
            update newMsg newModel

    Changed newRating ->
      { model | rating = newRating }

    Hovered transientRating ->
      { model | maybeTransientRating = Just transientRating }

    Left ->
      { model | maybeTransientRating = Nothing }


-- VIEW


view : Model -> Html Msg
view { rater, rating, maybeTransientRating } =
  div []
    [ h1 [] [ text "Elm Rater Example" ]

    , Html.map NewRaterMsg (Rater.view rating rater)
    , p [] [ text <| "Rating: " ++ String.fromInt rating ]
    , p []
        [ text <| "Transient rating: " ++ (
            case maybeTransientRating of
              Nothing ->
                "-"

              Just transientRating ->
                String.fromInt transientRating
          )
        ]
    ]
