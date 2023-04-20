module Pages.Assignment exposing (..)

import ApiClient.Assignments exposing (Assignment, getAssignment)
import ApiClient.Attempts exposing (Attempts, getSubmissionAttempts)
import ApiClient.Submissions exposing (Submission, Submissions, getAssignmentSubmissions, getUserSubmission)
import ApiClient.Users exposing (User, UserType(..), Users, getClassroomUsers, getUser)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (a, button, div, h1, h2, li, p, span, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import List exposing (filter, map)
import String exposing (fromInt)
import Util exposing (Either(..), findBy, loadingIfNothing)



-- MODEL


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId assignmentId =
    ( Model navKey userId Nothing assignmentId Nothing Nothing Nothing Nothing Nothing
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
    , userSubmission : Maybe (Either Submission Bool)
    , userAttempts : Maybe Attempts
    }



-- VIEW


view : Model -> Document Msg
view { userId, userData, assignmentData, participantsData, participantsSubmissions, userSubmission, userAttempts } =
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
                                        , loadingIfNothing userSubmission <|
                                            \submission ->
                                                case submission of
                                                    Left _ ->
                                                        loadingIfNothing userAttempts <|
                                                            \attempts ->
                                                                div []
                                                                    [ ul [] <| map (\attempt -> li [] [ text <| "attempt: " ++ fromInt attempt.id ]) attempts
                                                                    , button [ onClick AddAttemptClicked ] [ text "New attempt" ]
                                                                    ]

                                                    -- If there is no submission, place a button to add one
                                                    Right _ ->
                                                        button [] [ text "Add submission" ]
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
                                                                                    , a
                                                                                        [ href <| "/users/" ++ fromInt userId ++ "/submissions/" ++ fromInt submission.id ++ "/grade" ]
                                                                                        [ text "Grade" ]
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
        UsersMsg (ApiClient.Users.DataReceived result) ->
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

                -- TODO: Handle
                _ ->
                    ( model, Cmd.none )

        UsersMsg (ApiClient.Users.ClassroomUsersDataReceived result) ->
            case result of
                Ok participants ->
                    ( { model | participantsData = Just participants }
                    , Cmd.map SubmissionsMsg <| getAssignmentSubmissions model.assignmentId
                    )

                -- TODO: Handle
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

                -- TODO: Handle
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
                            , Cmd.map AttemptsMsg <| getSubmissionAttempts submission.id
                            )

                        _ ->
                            ( { model | userSubmission = Just <| Right False }, Cmd.none )

                -- TODO: Handle
                _ ->
                    ( model, Cmd.none )

        SubmissionsMsg (ApiClient.Submissions.ReceivedAssignmentSubmissions result) ->
            case result of
                Ok submissions ->
                    ( { model | participantsSubmissions = Just submissions }, Cmd.none )

                -- TODO: Handle
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

                -- TODO: Handle
                _ ->
                    ( model, Cmd.none )

        AddAttemptClicked ->
            ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId ++ "/addAttempt" )

        AddSubmissionClicked ->
            ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId ++ "/addSubmission" )


type Msg
    = AddAttemptClicked
    | AddSubmissionClicked
    | UsersMsg ApiClient.Users.Msg
    | AssignmentsMsg ApiClient.Assignments.Msg
    | SubmissionsMsg ApiClient.Submissions.Msg
    | AttemptsMsg ApiClient.Attempts.Msg
