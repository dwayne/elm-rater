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

  , rater4 : Rater.State
  , rating4 : Rating

  , rater5 : Rater.State
  , rating5 : Rating
  , rater5MaybeTransientValue : Maybe Int

  , rating6 : Rating
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
        , maybeOnClear = Just ClearedRater4
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
        , maybeOnClear = Nothing
        , onHover = HoveredOverRater5
        , onLeave = LeftRater5
        }
        model.rater5
        model.rating5

    , h2 [] [ text "Read only" ]
    , Rater.viewReadOnly model.rating6

    , h2 [] [ text "Disabled" ]
    , Rater.viewDisabled model.rating6
    ]
