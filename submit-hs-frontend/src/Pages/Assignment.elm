module Pages.Assignment exposing (..)

import ApiClient.Assignments exposing (Assignment, getAssignment)
import ApiClient.Users exposing (User, UserType(..), Users, getClassroomUsers, getUser)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (button, div, h1, h2, li, p, text, ul)
import List exposing (filter, map)
import String exposing (fromInt)
import Util exposing (loadingIfNothing)



-- MODEL


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId assignmentId =
    ( Model navKey userId Nothing assignmentId Nothing Nothing
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
    }



-- VIEW


view : Model -> Document Msg
view { userData, assignmentData, participantsData } =
    { title = "Assignment"
    , body =
        [ loadingIfNothing assignmentData
            (\assignment ->
                div []
                    [ h1 [] [ text ("Assignment " ++ fromInt assignment.id) ]
                    , p [] [ text assignment.description ]
                    , loadingIfNothing userData
                        (\user ->
                            case user.userType of
                                Student ->
                                    -- TODO: First check for submissions, then add submission if its not there, otherwise
                                    button [] [ text "Add submission" ]

                                _ ->
                                    div []
                                        [ h2 [] [ text "Participants" ]
                                        , loadingIfNothing participantsData
                                            (\participants ->
                                                ul [] <|
                                                    map
                                                        (\participant ->
                                                            -- TODO show if user has a submission or not, and add a link to the submission
                                                            li [] [ text participant.userName ]
                                                        )
                                                    <|
                                                        filter
                                                            (\p -> p.userType == Student)
                                                            participants
                                            )
                                        ]
                        )
                    ]
            )
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
                                    Cmd.none

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
                    , Cmd.none
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
                                    Cmd.none

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


type Msg
    = UsersMsg ApiClient.Users.Msg
    | AssignmentsMsg ApiClient.Assignments.Msg
