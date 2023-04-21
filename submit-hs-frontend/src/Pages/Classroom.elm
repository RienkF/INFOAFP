module Pages.Classroom exposing (..)

import ApiClient.Assignments exposing (Assignments, getClassroomAssignments)
import ApiClient.Classrooms exposing (Classroom, getClassroom)
import ApiClient.Users exposing (User, UserType(..), Users, getClassroomUsers, getUser, userTypeToString)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (a, button, div, h1, h2, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import List exposing (map)
import String exposing (fromInt)
import Util exposing (loadingIfNothing)



-- MODEL


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId classroomId =
    ( Model navKey userId Nothing classroomId Nothing Nothing Nothing
    , Cmd.batch
        [ Cmd.map UsersMsg (getUser userId)
        , Cmd.map ClassroomsMsg (getClassroom classroomId)
        , Cmd.map UsersMsg (getClassroomUsers classroomId)
        , Cmd.map AssignmentsMsg (getClassroomAssignments classroomId)
        ]
    )


type alias Model =
    { navKey : Key
    , userId : Int
    , userData : Maybe User
    , classroomId : Int
    , classroomData : Maybe Classroom
    , participantsData : Maybe Users
    , assignmentsData : Maybe Assignments
    }



-- VIEW


view : Model -> Document Msg
view { userId, userData, classroomData, assignmentsData, participantsData } =
    { title = "Classroom"
    , body =
        [ loadingIfNothing userData
            (\user ->
                div []
                    [ loadingIfNothing classroomData
                        (\classroom ->
                            h1 [] [ text ("Classroom: " ++ classroom.name) ]
                        )
                    , h2 [] [ text "Participants" ]
                    , loadingIfNothing participantsData
                        (\participants ->
                            ul [] <|
                                map
                                    (\participant ->
                                        li [] [ text <| participant.userName ++ " (" ++ userTypeToString participant.userType ++ ")" ]
                                    )
                                    participants
                        )
                    , h2 [] [ text "Assignments" ]
                    , loadingIfNothing assignmentsData
                        (\assignments ->
                            ul [] <|
                                map
                                    (\assignment ->
                                        li []
                                            [ a
                                                [ href <| "/users/" ++ fromInt userId ++ "/assignments/" ++ fromInt assignment.id ]
                                                [ text <| fromInt assignment.id ++ ": " ++ assignment.description ]
                                            ]
                                    )
                                    assignments
                        )
                    , div []
                        (case user.userType of
                            Teacher ->
                                [ button [ onClick AddParticipantClicked ] [ text "Add participant" ]
                                , button [ onClick AddAssignmentClicked ] [ text "Add assignment" ]
                                ]

                            _ ->
                                []
                        )
                    ]
            )
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
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg (ApiClient.Users.ClassroomUsersDataReceived result) ->
            case result of
                Ok participants ->
                    ( { model | participantsData = Just participants }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg _ ->
            ( model, Cmd.none )

        ClassroomsMsg (ApiClient.Classrooms.DataReceived result) ->
            case result of
                Ok [ classroom ] ->
                    ( { model | classroomData = Just classroom }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ClassroomsMsg _ ->
            ( model, Cmd.none )

        AssignmentsMsg (ApiClient.Assignments.DataReceived result) ->
            case result of
                Ok assignments ->
                    ( { model | assignmentsData = Just assignments }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AssignmentsMsg _ ->
            ( model, Cmd.none )

        AddParticipantClicked ->
            ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/classrooms/" ++ fromInt model.classroomId ++ "/addParticipant" )

        AddAssignmentClicked ->
            ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/classrooms/" ++ fromInt model.classroomId ++ "/assignments/add" )


type Msg
    = UsersMsg ApiClient.Users.Msg
    | ClassroomsMsg ApiClient.Classrooms.Msg
    | AssignmentsMsg ApiClient.Assignments.Msg
    | AddAssignmentClicked
    | AddParticipantClicked
