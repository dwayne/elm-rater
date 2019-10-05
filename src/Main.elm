module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, input, p, text)
import Html.Attributes as Attributes
import Html.Events as Events

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
  | NewInput String


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

    NewInput newRatingString ->
      case String.toInt newRatingString of
        Nothing ->
          model

        Just newRating ->
          { model | rating = newRating }


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

    , h2 [] [ text "What does ownership get us?" ]
    , p []
        [ text <| String.join " "
            [ "Since the rater doesn't own the rating"
            , "we can control the rating in other ways."
            ]
        ]
    , p []
        [ text "For e.g. use this input field to change the rating: "
        , input
            [ Attributes.type_ "number"
            , Attributes.value (String.fromInt rating)
            , Events.onInput NewInput
            ]
            []
        , text ". And watch how the rater updates itself."
        ]
    , p []
        [ text <| String.join " "
            [ "This can also be done in the case where the rater owns the"
            , "rating but we would have to expose a setter function on the"
            , "rater's API to update the rater's rating when another element"
            , "on the page wants to control the rating."
            ]
        ]
    ]
