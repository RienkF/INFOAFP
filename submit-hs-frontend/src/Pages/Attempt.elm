module Pages.Attempt exposing (..)

import ApiClient.Attempts exposing (Attempt, Msg(..), deleteAttempt, getAttempt)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, back)
import Html exposing (button, div, h1, p, text)
import Html.Events exposing (onClick)
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
                    div []
                        [ p [] [ text attempt.file ]
                        , button
                            [ onClick DeleteMsg ]
                            [ text "Delete Attempt" ]
                        ]
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

                _ ->
                    ( model, Cmd.none )

        AttemptsMsg _ ->
            ( model, Cmd.none )

        DeleteMsg ->
            ( model, Cmd.map DeleteResultMsg (deleteAttempt model.attemptId) )

        DeleteResultMsg (AttemptDeleted result) ->
            case result of
                Ok _ ->
                    ( model
                    , back model.navKey 1
                    )

                Err _ ->
                    ( model, Cmd.none )

        DeleteResultMsg _ ->
            ( model, Cmd.none )


type Msg
    = AttemptsMsg ApiClient.Attempts.Msg
    | DeleteMsg
    | DeleteResultMsg ApiClient.Attempts.Msg
