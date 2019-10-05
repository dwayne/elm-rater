module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, input, p, span, text)
import Html.Attributes as Attributes
import Html.Events as Events

import Rater exposing (defaultUpdateConfig, defaultViewConfig)
import Rating exposing (Rating)


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
  , rating : Rating
  , maybeTransientValue : Maybe Int
  }


init : Model
init =
  { rater = Rater.initial
  , rating = Rating.new 3 5
  , maybeTransientValue = Nothing
  }


-- UPDATE


type Msg
  = NewRaterMsg Rater.Msg
  | Changed Int
  | Hovered Int
  | Left
  | NewInput String
  | NoOp


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

    Changed newValue ->
      { model | rating = Rating.change newValue model.rating }

    Hovered transientValue ->
      { model | maybeTransientValue = Just transientValue }

    Left ->
      { model | maybeTransientValue = Nothing }

    NewInput newValueString ->
      case String.toInt newValueString of
        Nothing ->
          model

        Just newValue ->
          { model | rating = Rating.change newValue model.rating }

    NoOp ->
      model


-- VIEW


view : Model -> Html Msg
view { rater, rating, maybeTransientValue } =
  div []
    [ h1 [] [ text "Elm Rater Example" ]

    , Html.map NewRaterMsg (Rater.view rating rater)
    , p [] [ text <| "Rating: " ++ String.fromInt (Rating.value rating) ]
    , p []
        [ text <| "Transient rating: " ++ (
            case maybeTransientValue of
              Nothing ->
                "-"

              Just transientValue ->
                String.fromInt transientValue
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
            , Attributes.value (String.fromInt (Rating.value rating))
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

    , h2 [] [ text "Read only" ]
    , p [] [ Html.map (always NoOp) (Rater.viewReadOnly (Rating.new 2 10)) ]
    , p []
        [ Html.map (always NoOp) <|
            Rater.viewReadOnlyCustom
              { defaultViewConfig
              | selected = text << String.fromInt
              , unselected = always (text "_")
              }
              (Rating.new 5 10)
        ]

    , h2 [] [ text "Disabled" ]
    , p [] [ Html.map (always NoOp) (Rater.viewDisabled (Rating.new 7 15)) ]
    , p []
        [ Html.map (always NoOp) <|
            Rater.viewDisabledCustom
              { defaultViewConfig
              | selected = \_ ->
                  span
                    [ Attributes.style "color" "red" ]
                    [ text "\u{2764}" ]
              , unselected = \_ -> text "\u{2764}"
              }
              (Rating.new 5 15)
        ]
    ]
