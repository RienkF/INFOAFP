module Pages.Attempt exposing (..)

import ApiClient.Assignments
import ApiClient.Attempts exposing (Attempt, getAttempt)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (a, button, div, h1, h2, li, p, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import List exposing (map)
import String exposing (fromInt)
import Util exposing (loadingIfNothing)



-- MODEL


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId attemptId =
    ( Model navKey userId attemptId Nothing
    , Cmd.batch
        [ Cmd.map AttemptsMsg (getAttempt attemptId)
        ]
    )


type alias Model =
    { navKey : Key
    , userId : Int
    , attemptId : Int
    , attemptData : Maybe Attempt
    }



-- VIEW


view : Model -> Document Msg
view { attemptId, attemptData } =
    { title = "Classroom"
    , body =
        [ div []
            [ h1 [] [ text ("Attempt: " ++ fromInt attemptId) ]
            , loadingIfNothing attemptData
                (\attempt ->
                    p [] [ text attempt.file ]
                )
            ]
        ]
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptsMsg (ApiClient.Attempts.ReceivedAttempt result) ->
            case result of
                Ok [ attempt ] ->
                    ( { model | attemptData = Just attempt }
                    , Cmd.none
                    )

                -- TODO: Handle
                _ ->
                    ( model, Cmd.none )

        AttemptsMsg _ ->
            ( model, Cmd.none )


type Msg
    = AttemptsMsg ApiClient.Attempts.Msg
