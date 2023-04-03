module Pages.Login exposing (..)

import ApiClient.Users as Users exposing (Model(..), Msg(..), Users, getUsers)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append)
import Platform.Cmd exposing (none)
import RouteEvent exposing (RouteEvent(..))
import String exposing (fromInt, toInt)
import Util exposing (isNothing)



-- MODEL


type alias Model =
    { selectedUserId : Maybe Int, userOptions : Maybe Users }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing, Cmd.map UsersMsg getUsers )



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
        [ select
            [ value selectedValue, onInput (\x -> UpdateUser (toInt x)) ]
            options
        , button
            [ onClick SubmitUser, disabled (isNothing selectedUserId) ]
            [ text "Ok" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateUser (Maybe Int)
    | SubmitUser
    | UsersMsg Users.Msg


update : Msg -> Model -> ( Model, Cmd Msg, RouteEvent )
update msg model =
    case msg of
        UsersMsg (DataReceived result) ->
            case result of
                Ok users ->
                    ( { model | userOptions = Just users }
                    , Cmd.none
                    , NoEvent
                    )

                -- TODO: Handle
                Err _ ->
                    ( model, Cmd.none, NoEvent )

        UsersMsg _ ->
            ( model, none, NoEvent )

        UpdateUser user ->
            ( { model | selectedUserId = user }, none, NoEvent )

        SubmitUser ->
            case model.selectedUserId of
                Nothing ->
                    ( model, none, NoEvent )

                Just userId ->
                    ( model, none, ToClassrooms userId )
