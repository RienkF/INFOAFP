module Pages.Login exposing (..)

import ApiClient.Users as Users exposing (Model(..), Msg(..), Users, getUsers)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (none)
import String exposing (fromInt)



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
                    List.map (\{ userName, id } -> option [ value (fromInt id) ] [ text userName ]) userValues

                Nothing ->
                    []
    in
    { title = "Login"
    , body =
        [ select
            [ value selectedValue ]
            -- TODO: Load options here
            options
        , button
            [ onClick SubmitUser ]
            [ text "Ok" ]
        ]
    }



-- UPDATE


type Msg
    = SubmitUser
    | UsersMsg Users.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersMsg (DataReceived result) ->
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

        _ ->
            ( model, none )



-- TODO: Handle
