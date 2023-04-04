module Pages.Register exposing (..)

import ApiClient.Users as Users exposing (Msg(..), UserType(..), getUsers, stringToUserType, userTypeToString)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (none)
import RouteEvent exposing (RouteEvent(..))



-- MODEL


type alias Model =
    { username : String, userType : UserType }


init : ( Model, Cmd Msg )
init =
    ( Model "" Student, Cmd.map UsersMsg getUsers )



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
    | UsersMsg Users.Msg


update : Msg -> Model -> ( Model, Cmd Msg, RouteEvent )
update msg model =
    case msg of
        UsersMsg (UserCreated result) ->
            case result of
                Ok _ ->
                    ( model
                    , Cmd.none
                    , ToLogin
                    )

                -- TODO: Handle
                Err _ ->
                    ( model, Cmd.none, NoEvent )

        UsersMsg _ ->
            ( model, none, NoEvent )

        UpdateUserName userName ->
            ( { model | username = userName }, none, NoEvent )

        UpdateUserType userType ->
            case userType of
                Just userTypeValue ->
                    ( { model | userType = userTypeValue }, none, NoEvent )

                Nothing ->
                    ( model, none, NoEvent )

        RegisterUser ->
            ( model, Cmd.map UsersMsg (Users.createUser model.username model.userType), NoEvent )
