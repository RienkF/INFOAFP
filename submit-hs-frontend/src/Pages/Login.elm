module Pages.Login exposing (..)

import ApiClient.Users exposing (Msg(..), Users, getUsers)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append)
import Platform.Cmd exposing (none)
import String exposing (fromInt, toInt)
import Util exposing (isNothing)
import ApiClient.Users exposing (deleteUser)
import Browser.Navigation exposing (reload)



-- MODEL


type alias Model =
    { navKey : Key, selectedUserId : Maybe Int, userOptions : Maybe Users }


init : Key -> ( Model, Cmd Msg )
init navKey =
    ( Model navKey Nothing Nothing, Cmd.map UsersMsg getUsers )



-- VIEW


view : Model -> Document Msg
view { selectedUserId, userOptions } =
    let
        selectedValue =
            case selectedUserId of
                Just id ->
                    fromInt id

                Nothing ->
                    ""

        options =
            case userOptions of
                Just userValues ->
                    append
                        [ option [ value "" ] [ text "No user selected" ] ]
                        (List.map (\{ userName, id } -> option [ value (fromInt id) ] [ text userName ]) userValues)

                Nothing ->
                    []
    in
    { title = "Login"
    , body =
        [ h1 [] [ text "Submit-hs" ]
        , h2 [] [ text "Please select user" ]
        , select
            [ value selectedValue, onInput (\x -> UpdateUser (toInt x)) ]
            options
        , button
            [ onClick SubmitUser, disabled (isNothing selectedUserId) ]
            [ text "Login" ]
        , button
            [ onClick Delete, disabled (isNothing selectedUserId) ]
            [ text "Delete User" ]
        , h2 [] [ text "Or click here to register" ]
        , button
            [ onClick RegisterClicked ]
            [ text "Register" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateUser (Maybe Int)
    | SubmitUser
    | UsersMsg ApiClient.Users.Msg
    | RegisterClicked
    | Delete
    | DeleteResult ApiClient.Users.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersMsg (UsersDataReceived result) ->
            case result of
                Ok users ->
                    ( { model | userOptions = Just users }
                    , Cmd.none
                    )

                -- TODO: Handle
                Err _ ->
                    ( model, Cmd.none )

        UsersMsg _ ->
            ( model, none )

        UpdateUser user ->
            ( { model | selectedUserId = user }, none )

        SubmitUser ->
            case model.selectedUserId of
                Nothing ->
                    ( model, none )

                Just userId ->
                    ( model, pushUrl model.navKey ("/users/" ++ fromInt userId ++ "/classrooms") )

        RegisterClicked ->
            ( model, pushUrl model.navKey "/register" )

        Delete ->
            case model.selectedUserId of
                Just id -> ( model, Cmd.map DeleteResult (deleteUser id))
                _ -> ( model, Cmd.none )
        DeleteResult (UserDeleted result) ->
            case result of
                Ok () -> ( model, reload )
                _ -> ( model, Cmd.none )
        DeleteResult _ ->
                ( model, Cmd.none )
