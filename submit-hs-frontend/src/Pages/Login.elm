module Pages.Login exposing (..)

import ApiClient.Users as Users exposing (Msg(..), Users, getUsers)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
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
        , h2 [] [ text "Please log in" ]
        , select
            [ value selectedValue, onInput (\x -> UpdateUser (toInt x)) ]
            options
        , button
            [ onClick SubmitUser, disabled (isNothing selectedUserId) ]
            [ text "Ok" ]
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
    | UsersMsg Users.Msg
    | RegisterClicked


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

        UpdateUser user ->
            ( { model | selectedUserId = user }, none )

        SubmitUser ->
            case model.selectedUserId of
                Nothing ->
                    ( model, none )

                Just userId ->
                    ( model, pushUrl model.navKey ("/classrooms/" ++ fromInt userId) )

        RegisterClicked ->
            ( model, pushUrl model.navKey "/register" )
