module Pages.Grade exposing (..)

import ApiClient.Attempts exposing (Attempt)
import ApiClient.Grading exposing (Msg(..))
import ApiClient.Submissions exposing (Msg(..), Submission, getSubmission)
import ApiClient.Users exposing (Msg(..), UserType(..))
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (isEmpty)
import Platform.Cmd exposing (none)
import String exposing (fromInt)
import Util exposing (Either(..), loadingIfNothing)



-- MODEL


type alias Model =
    { navKey : Key
    , submissionId : Int
    , submissionData : Maybe Submission
    , reviewerId : Int
    , grade : String
    , feedback : String
    , attempt : Maybe (Either Attempt Bool)
    , alreadyGraded : Maybe Bool
    }


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey reviewerId submissionId =
    ( Model navKey submissionId Nothing reviewerId "0.0" "No feedback" Nothing Nothing
    , Cmd.batch
        [ Cmd.map AttemptsMsg (ApiClient.Attempts.getSubmissionAttempts submissionId)
        , Cmd.map SubmissionsMsg <| getSubmission submissionId
        , Cmd.map GradingMsg (ApiClient.Grading.getGradings submissionId)
        ]
    )



-- VIEW


view : Model -> Document Msg
view { grade, feedback, attempt, alreadyGraded } =
    { title = "Grade"
    , body =
        [ h1 [] [ text "Grade" ]
        , loadingIfNothing attempt <|
            \attemptMaybe ->
                case attemptMaybe of
                    Left submission ->
                        div []
                            [ h2 [] [ text "File" ]
                            , p [] [ text submission.file ]
                            , loadingIfNothing alreadyGraded <|
                                \alreadyGradedMaybe ->
                                    if alreadyGradedMaybe then
                                        h2 [] [ text "Already graded" ]

                                    else
                                        div []
                                            [ h2 [] [ text "Set grade" ]
                                            , input
                                                [ value grade, onInput UpdateGrade ]
                                                []
                                            , br [] []
                                            , br [] []
                                            , input
                                                [ value feedback, onInput UpdateFeedback ]
                                                []
                                            , br [] []
                                            , br [] []
                                            , button
                                                [ onClick ChangeGrade ]
                                                [ text "Grade" ]
                                            ]
                            ]

                    Right _ ->
                        p [] [ text "error: no submission for this assignment found" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateGrade String
    | UpdateFeedback String
    | ChangeGrade
    | GradingMsg ApiClient.Grading.Msg
    | AttemptsMsg ApiClient.Attempts.Msg
    | SubmissionsMsg ApiClient.Submissions.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptsMsg (ApiClient.Attempts.ReceivedSubmissionAttempts result) ->
            case result of
                Ok attempts ->
                    case attempts of
                        attempt :: _ ->
                            ( { model | attempt = Just (Left attempt) }, none )

                        _ ->
                            ( { model | attempt = Just (Right False) }, none )

                _ ->
                    ( model, none )

        GradingMsg (ApiClient.Grading.ReceivedGradings result) ->
            case result of
                Ok gradings ->
                    ( { model | alreadyGraded = Just (not (isEmpty gradings)) }, none )

                _ ->
                    ( model, none )

        GradingMsg (GradingCreated _) ->
            ( model
            , pushUrl model.navKey <|
                "/users/"
                    ++ fromInt model.reviewerId
                    ++ "/assignments/"
                    ++ (case model.submissionData of
                            Just submission ->
                                fromInt submission.assignmentId

                            Nothing ->
                                ""
                       )
            )

        GradingMsg _ ->
            ( model, none )

        AttemptsMsg _ ->
            ( model, none )

        UpdateGrade grade ->
            ( { model | grade = grade }, none )

        UpdateFeedback feedback ->
            ( { model | feedback = feedback }, none )

        ChangeGrade ->
            ( model, Cmd.map GradingMsg (ApiClient.Grading.addGrade model.submissionId model.reviewerId model.grade model.feedback) )

        SubmissionsMsg (ReceivedSubmissions result) ->
            case result of
                Ok [ submission ] ->
                    ( { model | submissionData = Just submission }, none )

                _ ->
                    ( model, none )

        SubmissionsMsg _ ->
            ( model, none )
