-- Main module, took a lot of inspiration from https://github.com/rtfeldman/elm-spa-example/blob/master/src/Main.elm


module Main exposing (main, useInit)

import Browser exposing (Document, application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (div)
import Html.Attributes exposing (style)
import Pages.AddAssignment
import Pages.AddAttempt
import Pages.AddClassroom
import Pages.AddParticipant
import Pages.AddSubmission
import Pages.Assignment
import Pages.Attempt
import Pages.Classroom
import Pages.Classrooms exposing (Model, Msg)
import Pages.Login exposing (Model, Msg(..), init)
import Pages.Register
import Platform.Cmd
import Platform.Sub
import Route exposing (Route(..))
import Url exposing (Url)



-- MODEL


type Model
    = LoginModel Pages.Login.Model
    | RegisterModel Pages.Register.Model
    | ClassroomsModel Pages.Classrooms.Model
    | ClassroomModel Pages.Classroom.Model
    | AddClassroomModel Pages.AddClassroom.Model
    | AddParticipantModel Pages.AddParticipant.Model
    | AddAssignmentModel Pages.AddAssignment.Model
    | AssignmentModel Pages.Assignment.Model
    | AddSubmissionModel Pages.AddSubmission.Model
    | AddAttemptModel Pages.AddAttempt.Model
    | AttemptModel Pages.Attempt.Model


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ _ key =
    useInit
        (Pages.Login.init key)
        LoginModel
        LoginMsg


useInit : ( a, Cmd b ) -> (a -> Model) -> (b -> Msg) -> ( Model, Cmd Msg )
useInit ( initModel, initCmd ) fModel fCmd =
    ( fModel initModel, Cmd.map fCmd initCmd )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        LoginModel loginModel ->
            useView (Pages.Login.view loginModel) LoginMsg

        RegisterModel registerModel ->
            useView (Pages.Register.view registerModel) RegisterMsg

        ClassroomsModel classroomsModel ->
            useView (Pages.Classrooms.view classroomsModel) ClassroomsMsg

        ClassroomModel classroomModel ->
            useView (Pages.Classroom.view classroomModel) ClassroomMsg

        AddClassroomModel addClassroomsModel ->
            useView (Pages.AddClassroom.view addClassroomsModel) AddClassroomMsg

        AddParticipantModel addParticipantModel ->
            useView (Pages.AddParticipant.view addParticipantModel) AddParticipantMsg

        AddAssignmentModel addAssignmentModel ->
            useView (Pages.AddAssignment.view addAssignmentModel) AddAssignmentMsg

        AssignmentModel assignmentModel ->
            useView (Pages.Assignment.view assignmentModel) AssignmentMsg

        AddSubmissionModel addSubmissionModel ->
            useView (Pages.AddSubmission.view addSubmissionModel) AddSubmissionMsg

        AddAttemptModel addAttemptModel ->
            useView (Pages.AddAttempt.view addAttemptModel) AddAttemptMsg

        AttemptModel attemptModel ->
            useView (Pages.Attempt.view attemptModel) AttemptMsg


useView : Document a -> (a -> Msg) -> Document Msg
useView result mapMsg =
    { title = result.title, body = [ div [ style "padding-left" "10px" ] (List.map (Html.map mapMsg) result.body) ] }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | LoginMsg Pages.Login.Msg
    | RegisterMsg Pages.Register.Msg
    | ClassroomsMsg Pages.Classrooms.Msg
    | ClassroomMsg Pages.Classroom.Msg
    | AddClassroomMsg Pages.AddClassroom.Msg
    | AddParticipantMsg Pages.AddParticipant.Msg
    | AddAssignmentMsg Pages.AddAssignment.Msg
    | AssignmentMsg Pages.Assignment.Msg
    | AddSubmissionMsg Pages.AddSubmission.Msg
    | AddAttemptMsg Pages.AddAttempt.Msg
    | AttemptMsg Pages.Attempt.Msg
    | NoMsg


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            Pages.Login.init (getKey model)
                |> updateWith LoginModel LoginMsg model

        Just Route.Login ->
            Pages.Login.init (getKey model)
                |> updateWith LoginModel LoginMsg model

        Just Route.Register ->
            Pages.Register.init (getKey model)
                |> updateWith RegisterModel RegisterMsg model

        Just (Route.Classrooms userId) ->
            Pages.Classrooms.init (getKey model) userId
                |> updateWith ClassroomsModel ClassroomsMsg model

        Just (Route.Classroom userId classroomId) ->
            Pages.Classroom.init (getKey model) userId classroomId
                |> updateWith ClassroomModel ClassroomMsg model

        Just (Route.AddClassroom userId) ->
            Pages.AddClassroom.init (getKey model) userId
                |> updateWith AddClassroomModel AddClassroomMsg model

        Just (Route.AddParticipant userId classroomId) ->
            Pages.AddParticipant.init (getKey model) userId classroomId
                |> updateWith AddParticipantModel AddParticipantMsg model

        Just (Route.AddAssignment userId classroomId) ->
            Pages.AddAssignment.init (getKey model) userId classroomId
                |> updateWith AddAssignmentModel AddAssignmentMsg model

        Just (Route.Assignment userId assignmentId) ->
            Pages.Assignment.init (getKey model) userId assignmentId
                |> updateWith AssignmentModel AssignmentMsg model

        Just (Route.AddSubmission userId assignmentId) ->
            Pages.AddSubmission.init (getKey model) userId assignmentId
                |> updateWith AddSubmissionModel AddSubmissionMsg model

        Just (Route.AddAttempt userId assignmentId) ->
            Pages.AddAttempt.init (getKey model) userId assignmentId
                |> updateWith AddAttemptModel AddAttemptMsg model

        Just (Route.Attempt userId attemptId) ->
            Pages.Attempt.init (getKey model) userId attemptId
                |> updateWith AttemptModel AttemptMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , pushUrl (getKey model) url.path
                    )

                Browser.External href ->
                    ( model
                    , load href
                    )

        ( LoginMsg loginMsg, LoginModel loginModel ) ->
            Pages.Login.update loginMsg loginModel
                |> updateWith LoginModel LoginMsg model

        ( RegisterMsg registerMsg, RegisterModel registerModel ) ->
            Pages.Register.update registerMsg registerModel
                |> updateWith RegisterModel RegisterMsg model

        ( ClassroomsMsg classroomsMsg, ClassroomsModel classroomsModel ) ->
            Pages.Classrooms.update classroomsMsg classroomsModel
                |> updateWith ClassroomsModel ClassroomsMsg model

        ( ClassroomMsg classroomMsg, ClassroomModel classroomModel ) ->
            Pages.Classroom.update classroomMsg classroomModel
                |> updateWith ClassroomModel ClassroomMsg model

        ( AddClassroomMsg addClassroomMsg, AddClassroomModel addClassroomsModel ) ->
            Pages.AddClassroom.update addClassroomMsg addClassroomsModel
                |> updateWith AddClassroomModel AddClassroomMsg model

        ( AddParticipantMsg addParticipantMsg, AddParticipantModel addParticipantModel ) ->
            Pages.AddParticipant.update addParticipantMsg addParticipantModel
                |> updateWith AddParticipantModel AddParticipantMsg model

        ( AddAssignmentMsg addAssignmentMsg, AddAssignmentModel addAssignmentModel ) ->
            Pages.AddAssignment.update addAssignmentMsg addAssignmentModel
                |> updateWith AddAssignmentModel AddAssignmentMsg model

        ( AssignmentMsg assignmentMsg, AssignmentModel assignmentModel ) ->
            Pages.Assignment.update assignmentMsg assignmentModel
                |> updateWith AssignmentModel AssignmentMsg model

        ( AddSubmissionMsg addSubmissionMsg, AddSubmissionModel addSubmissionModel ) ->
            Pages.AddSubmission.update addSubmissionMsg addSubmissionModel
                |> updateWith AddSubmissionModel AddSubmissionMsg model

        ( AddAttemptMsg addAttemptMsg, AddAttemptModel addAttemptModel ) ->
            Pages.AddAttempt.update addAttemptMsg addAttemptModel
                |> updateWith AddAttemptModel AddAttemptMsg model

        ( AttemptMsg attemptMsg, AttemptModel attemptModel ) ->
            Pages.Attempt.update attemptMsg attemptModel
                |> updateWith AttemptModel AttemptMsg model

        ( NoMsg, _ ) ->
            ( model, Platform.Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


getKey : Model -> Key
getKey model =
    case model of
        LoginModel loginModel ->
            loginModel.navKey

        RegisterModel registerModel ->
            registerModel.navKey

        ClassroomsModel classroomsModel ->
            classroomsModel.navKey

        ClassroomModel classroomModel ->
            classroomModel.navKey

        AddClassroomModel addClassroomModel ->
            addClassroomModel.navKey

        AddParticipantModel addParticipantModel ->
            addParticipantModel.navKey

        AddAssignmentModel addAssignmentModel ->
            addAssignmentModel.navKey

        AssignmentModel assignmentModel ->
            assignmentModel.navKey

        AddSubmissionModel addSubmissionModel ->
            addSubmissionModel.navKey

        AddAttemptModel addAttemptModel ->
            addAttemptModel.navKey

        AttemptModel attemptModel ->
            attemptModel.navKey


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Platform.Sub.none



-- MAIN


main : Program () Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
