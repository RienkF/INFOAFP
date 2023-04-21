module Pages.Assignment exposing (..)

import ApiClient.Assignments exposing (Assignment, getAssignment)
import ApiClient.Attempts exposing (Attempts, getSubmissionAttempts)
import ApiClient.Grading exposing (Grading, Gradings, deleteGrade, getAssignmentGrades, getGradings)
import ApiClient.Submissions exposing (Submission, Submissions, getAssignmentSubmissions, getUserSubmission)
import ApiClient.Users exposing (User, UserType(..), Users, getClassroomUsers, getReviewer, getUser)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (a, button, div, h1, h2, h3, li, p, span, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import List exposing (filter, map, reverse, sortBy)
import Pages.Grade exposing (Msg(..))
import String exposing (fromFloat, fromInt)
import Util exposing (Either(..), findBy, loadingIfNothing)



-- MODEL


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId assignmentId =
    ( Model navKey userId Nothing assignmentId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , Cmd.batch
        [ Cmd.map UsersMsg (getUser userId)
        , Cmd.map AssignmentsMsg (getAssignment assignmentId)
        ]
    )


type alias Model =
    { navKey : Key
    , userId : Int
    , userData : Maybe User
    , assignmentId : Int
    , assignmentData : Maybe Assignment
    , participantsData : Maybe Users
    , participantsSubmissions : Maybe Submissions
    , participantsGrades : Maybe Gradings
    , userSubmission : Maybe (Either Submission Bool)
    , userAttempts : Maybe Attempts
    , userGrade : Maybe (Either Grading Bool)
    , reviewer : Maybe User
    }



-- VIEW


view : Model -> Document Msg
view { userId, userData, assignmentData, participantsData, participantsSubmissions, userSubmission, userAttempts, userGrade, reviewer, participantsGrades } =
    { title = "Assignment"
    , body =
        [ loadingIfNothing assignmentData <|
            \assignment ->
                div []
                    [ h1 [] [ text ("Assignment " ++ fromInt assignment.id) ]
                    , p [] [ text assignment.description ]
                    , loadingIfNothing userData <|
                        \user ->
                            case user.userType of
                                Student ->
                                    div []
                                        [ h2 [] [ text "My submission:" ]
                                        , h3 [] [ text "Grade:" ]
                                        , loadingIfNothing userGrade <|
                                            \grade ->
                                                case grade of
                                                    Left gradeVal ->
                                                        div []
                                                            [ p [] [ text <| "Score: " ++ fromFloat gradeVal.grade ]
                                                            , p [] [ text <| "Feedback: " ++ gradeVal.feedback ]
                                                            , p [] [ text "Reviewer: ", loadingIfNothing reviewer <| \reviewerVal -> text <| reviewerVal.userName ]
                                                            ]

                                                    Right _ ->
                                                        p [] [ text "No grade yet" ]
                                        , h3 [] [ text "Attempts:" ]
                                        , loadingIfNothing userSubmission <|
                                            \submission ->
                                                case submission of
                                                    Left _ ->
                                                        loadingIfNothing userAttempts <|
                                                            \attempts ->
                                                                let
                                                                    sortedAttempts =
                                                                        reverse <| sortBy (\attempt -> attempt.timeStamp) attempts
                                                                in
                                                                div []
                                                                    [ ul [] <|
                                                                        map
                                                                            (\attempt ->
                                                                                li
                                                                                    []
                                                                                    [ a
                                                                                        [ href <| "/users/" ++ fromInt userId ++ "/attempts/" ++ fromInt attempt.id ]
                                                                                        [ text <| attempt.timeStamp ++ " | attempt id: " ++ fromInt attempt.id ]
                                                                                    ]
                                                                            )
                                                                            sortedAttempts
                                                                    , button [ onClick AddAttemptClicked ] [ text "New attempt" ]
                                                                    ]

                                                    -- If there is no submission, place a button to add one
                                                    Right _ ->
                                                        button [ onClick AddSubmissionClicked ] [ text "Add submission" ]
                                        ]

                                _ ->
                                    div []
                                        [ h2 [] [ text "Participants" ]
                                        , loadingIfNothing participantsData <|
                                            \participants ->
                                                loadingIfNothing participantsSubmissions <|
                                                    \participantSubmissions ->
                                                        ul [] <|
                                                            map
                                                                (\participant ->
                                                                    let
                                                                        participantSubmission =
                                                                            findBy participantSubmissions (\x -> x.userId) participant.id
                                                                    in
                                                                    li []
                                                                        [ text <|
                                                                            participant.userName
                                                                                ++ " | "
                                                                        , case participantSubmission of
                                                                            Just submission ->
                                                                                span []
                                                                                    [ text "has submitted: "
                                                                                    , loadingIfNothing participantsGrades <|
                                                                                        \gradesData ->
                                                                                            let
                                                                                                submissionGradeMaybe =
                                                                                                    findBy gradesData (\grade -> grade.submission) submission.id
                                                                                            in
                                                                                            case submissionGradeMaybe of
                                                                                                Nothing ->
                                                                                                    a
                                                                                                        [ href <| "/users/" ++ fromInt userId ++ "/submissions/" ++ fromInt submission.id ++ "/grade" ]
                                                                                                        [ text "Grade" ]

                                                                                                Just grading ->
                                                                                                    div []
                                                                                                        [ span [] [ text "already graded" ]
                                                                                                        , button
                                                                                                            [ onClick (DeleteGradeClicked grading.id) ]
                                                                                                            [ text "Delete grade" ]
                                                                                                        ]
                                                                                    ]

                                                                            Nothing ->
                                                                                span [] [ text "No submission yet" ]
                                                                        ]
                                                                )
                                                            <|
                                                                filter
                                                                    (\p -> p.userType == Student)
                                                                    participants
                                        ]
                    ]
        ]
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersMsg (ApiClient.Users.UserDataReceived result) ->
            case result of
                Ok [ user ] ->
                    ( { model | userData = Just user }
                    , case model.assignmentData of
                        Just assignment ->
                            case user.userType of
                                Student ->
                                    Cmd.map SubmissionsMsg <| getUserSubmission user.id assignment.id

                                _ ->
                                    Cmd.map UsersMsg <| getClassroomUsers assignment.classroomId

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg (ApiClient.Users.ClassroomUsersDataReceived result) ->
            case result of
                Ok participants ->
                    ( { model | participantsData = Just participants }
                    , Cmd.map SubmissionsMsg <| getAssignmentSubmissions model.assignmentId
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg (ApiClient.Users.ReviewerDataReceived result) ->
            case result of
                Ok [ reviewer ] ->
                    ( { model | reviewer = Just reviewer }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg _ ->
            ( model, Cmd.none )

        AssignmentsMsg (ApiClient.Assignments.DataReceived result) ->
            case result of
                Ok [ assignment ] ->
                    ( { model | assignmentData = Just assignment }
                    , case model.userData of
                        Just user ->
                            case user.userType of
                                Student ->
                                    Cmd.map SubmissionsMsg <| getUserSubmission user.id assignment.id

                                _ ->
                                    Cmd.map UsersMsg <| getClassroomUsers assignment.classroomId

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AssignmentsMsg _ ->
            ( model, Cmd.none )

        SubmissionsMsg (ApiClient.Submissions.ReceivedUserSubmissions result) ->
            case result of
                Ok submissions ->
                    case submissions of
                        [ submission ] ->
                            ( { model | userSubmission = Just <| Left submission }
                            , Cmd.batch
                                [ Cmd.map AttemptsMsg <| getSubmissionAttempts submission.id
                                , Cmd.map GradingsMsg <| getGradings submission.id
                                ]
                            )

                        _ ->
                            ( { model | userSubmission = Just <| Right False, userGrade = Just <| Right False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmissionsMsg (ApiClient.Submissions.ReceivedAssignmentSubmissions result) ->
            case result of
                Ok submissions ->
                    ( { model | participantsSubmissions = Just submissions }, Cmd.map GradingsMsg <| getAssignmentGrades model.assignmentId )

                _ ->
                    ( model, Cmd.none )

        SubmissionsMsg _ ->
            ( model, Cmd.none )

        AttemptsMsg (ApiClient.Attempts.ReceivedSubmissionAttempts result) ->
            case result of
                Ok attempts ->
                    ( { model | userAttempts = Just attempts }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AttemptsMsg _ ->
            ( model, Cmd.none )

        GradingsMsg (ApiClient.Grading.ReceivedGradings result) ->
            case result of
                Ok grades ->
                    case grades of
                        [ grade ] ->
                            ( { model | userGrade = Just (Left grade) }
                            , Cmd.map UsersMsg <| getReviewer grade.user
                            )

                        _ ->
                            ( { model | userGrade = Just (Right False) }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        GradingsMsg (ApiClient.Grading.ReceivedAssignmentGradings result) ->
            case result of
                Ok grades ->
                    ( { model | participantsGrades = Just grades }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GradingsMsg (ApiClient.Grading.GradingDeleted result) ->
            case result of
                Ok () ->
                    ( model, Cmd.map AssignmentsMsg (getAssignment model.assignmentId) )

                _ ->
                    ( model, Cmd.none )

        GradingsMsg _ ->
            ( model, Cmd.none )

        AddAttemptClicked ->
            ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId ++ "/addAttempt" )

        AddSubmissionClicked ->
            ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId ++ "/addSubmission" )

        DeleteGradeClicked gradingId ->
            ( model, Cmd.map GradingsMsg (deleteGrade gradingId) )


type Msg
    = AddAttemptClicked
    | AddSubmissionClicked
    | UsersMsg ApiClient.Users.Msg
    | AssignmentsMsg ApiClient.Assignments.Msg
    | SubmissionsMsg ApiClient.Submissions.Msg
    | AttemptsMsg ApiClient.Attempts.Msg
    | GradingsMsg ApiClient.Grading.Msg
    | DeleteGradeClicked Int
