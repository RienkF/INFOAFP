module Pages.AddParticipant exposing (..)

import ApiClient.Classrooms exposing (Msg(..), addClassroomParticipant)
import ApiClient.Users exposing (Msg(..), Users, getClassroomUsers, getUsers)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append, filter, member)
import Platform.Cmd exposing (none)
import Route exposing (Route(..))
import String exposing (fromInt, toInt)
import Util exposing (isNothing)



-- MODEL


type alias Model =
    { navKey : Key
    , userId : Int
    , classroomId : Int
    , selectedUserId : Maybe Int
    , userOptions : Maybe Users
    , currentParticipants : Maybe Users
    }


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId classroomId =
    ( Model navKey userId classroomId Nothing Nothing Nothing
    , Cmd.batch
        [ Cmd.map UsersMsg getUsers, Cmd.map UsersMsg (getClassroomUsers classroomId) ]
    )



-- VIEW


view : Model -> Document Msg
view { selectedUserId, userOptions, currentParticipants } =
    let
        selectedValue =
            case selectedUserId of
                Just id ->
                    fromInt id

                Nothing ->
                    ""

        options =
            case ( userOptions, currentParticipants ) of
                ( Just userValues, Just currentParticipantValues ) ->
                    let
                        -- Only show users that are not already part of the classroom
                        possibleUsers =
                            filter (\{ id } -> not <| member id <| List.map (\participant -> participant.id) currentParticipantValues) userValues
                    in
                    append
                        [ option [ value "" ] [ text "No user selected" ] ]
                        (List.map (\{ userName, id } -> option [ value (fromInt id) ] [ text userName ]) possibleUsers)

                _ ->
                    []
    in
    { title = "Login"
    , body =
        [ h1 [] [ text "Add participant" ]
        , select
            [ value selectedValue, onInput (\x -> UpdateParticipant (toInt x)) ]
            options
        , button
            [ onClick SubmitParticipant, disabled (isNothing selectedUserId) ]
            [ text "Ok" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateParticipant (Maybe Int)
    | SubmitParticipant
    | UsersMsg ApiClient.Users.Msg
    | ClassroomsMsg ApiClient.Classrooms.Msg
    | RegisterClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersMsg (ApiClient.Users.UsersDataReceived result) ->
            case result of
                Ok users ->
                    ( { model | userOptions = Just users }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg (ClassroomUsersDataReceived result) ->
            case result of
                Ok participants ->
                    ( { model | currentParticipants = Just participants }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UsersMsg _ ->
            ( model, none )

        UpdateParticipant user ->
            ( { model | selectedUserId = user }, none )

        SubmitParticipant ->
            case model.selectedUserId of
                Nothing ->
                    ( model, none )

                Just userId ->
                    ( model, Cmd.map ClassroomsMsg <| addClassroomParticipant model.classroomId userId )

        ClassroomsMsg (ClassroomParticipantAdded result) ->
            case result of
                Ok _ ->
                    ( model, pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/classrooms/" ++ fromInt model.classroomId )

                _ ->
                    ( model, Cmd.none )

        ClassroomsMsg _ ->
            ( model, none )

        RegisterClicked ->
            ( model, pushUrl model.navKey "/register" )
