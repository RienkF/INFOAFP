module Pages.Register exposing (..)

import ApiClient.Users exposing (Msg(..), UserType(..), createUser, getUsers, stringToUserType, userTypeToString)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (none)
import RouteEvent exposing (RouteEvent(..))



-- MODEL


type alias Model =
    { navKey : Key, username : String, userType : UserType }


init : Key -> ( Model, Cmd Msg )
init navKey =
    ( Model navKey "" Student, Cmd.map UsersMsg getUsers )



-- VIEW


view : Model -> Document Msg
view { username, userType } =
    let
        options =
            [ option [ value "student" ] [ text "Student" ]
            , option [ value "teacher" ] [ text "Teacher" ]
            , option [ value "ta" ] [ text "TA" ]
            ]
    in
    { title = "Register"
    , body =
        [ h1 [] [ text "Register" ]
        , h2 [] [ text "Input username" ]
        , input
            [ value username, onInput UpdateUserName ]
            []
        , h2 [] [ text "Input user type" ]
        , select
            [ value (userTypeToString userType), onInput (\x -> UpdateUserType (stringToUserType x)) ]
            options
        , br [] []
        , br [] []
        , button
            [ onClick RegisterUser ]
            -- , disabled (isNothing selectedUserId) ]
            [ text "Create user" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateUserName String
    | UpdateUserType (Maybe UserType)
    | RegisterUser
    | UsersMsg ApiClient.Users.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersMsg (UserCreated result) ->
            case result of
                Ok _ ->
                    ( model
                    , pushUrl model.navKey "/login"
                    )

                -- TODO: Handle
                Err _ ->
                    ( model, Cmd.none )

        UsersMsg _ ->
            ( model, none )

        UpdateUserName userName ->
            ( { model | username = userName }, none )

        UpdateUserType userType ->
            case userType of
                Just userTypeValue ->
                    ( { model | userType = userTypeValue }, none )

                Nothing ->
                    ( model, none )

        RegisterUser ->
            ( model, Cmd.map UsersMsg (createUser model.username model.userType) )
