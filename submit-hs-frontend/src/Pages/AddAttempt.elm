module Pages.AddAttempt exposing (..)

import ApiClient.Attempts exposing (Msg(..), submitAttempt)
import ApiClient.Submissions exposing (Msg(..), Submission, getUserSubmission)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.Classrooms exposing (Msg(..))
import Platform.Cmd exposing (none)
import String exposing (fromInt)
import Util exposing (Either(..), loadingIfNothing)



-- MODEL


type alias Model =
    { navKey : Key
    , userId : Int
    , assignmentId : Int
    , submissionData : Maybe (Either Submission Bool)
    , fileData : String
    }


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId assignmentId =
    ( Model navKey userId assignmentId Nothing "", Cmd.map Submissionsmsg <| getUserSubmission userId assignmentId )



-- VIEW


view : Model -> Document Msg
view { fileData, submissionData } =
    { title = "Add attempt"
    , body =
        [ h1 [] [ text "Add an attempt" ]
        , loadingIfNothing submissionData <|
            \submissionMaybe ->
                case submissionMaybe of
                    Left _ ->
                        div []
                            [ h2 [] [ text "Input a the file content of the attempt" ]
                            , textarea
                                [ value fileData, onInput UpdateFile ]
                                []
                            , br [] []
                            , br [] []
                            , button
                                [ onClick SubmitAttempt ]
                                [ text "Submit attempt" ]
                            ]

                    Right _ ->
                        p [] [ text "error: no submission for this assignment found" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateFile String
    | SubmitAttempt
    | AttemptMsg ApiClient.Attempts.Msg
    | Submissionsmsg ApiClient.Submissions.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptMsg (AttemptCreated result) ->
            case result of
                Ok (Just _) ->
                    ( model
                    , pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId
                    )

                _ ->
                    ( model, Cmd.none )

        AttemptMsg _ ->
            ( model, none )

        Submissionsmsg (ReceivedUserSubmissions result) ->
            case result of
                Ok [ submission ] ->
                    ( { model | submissionData = Just (Left submission) }, none )

                _ ->
                    ( { model | submissionData = Just (Right False) }, none )

        Submissionsmsg _ ->
            ( model, none )

        UpdateFile fileData ->
            ( { model | fileData = fileData }, none )

        SubmitAttempt ->
            case model.submissionData of
                Just (Left submission) ->
                    ( model, Cmd.map AttemptMsg <| submitAttempt submission.id model.fileData )

                -- Can't submit if there is no submission
                _ ->
                    ( model, none )
