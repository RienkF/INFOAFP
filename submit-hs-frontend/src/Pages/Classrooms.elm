module Pages.Classrooms exposing (..)

import ApiClient.Classrooms exposing (Classrooms, getUserClassrooms)
import ApiClient.Users exposing (User, UserType(..), getUser)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (a, button, div, li, p, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import List exposing (map)
import String exposing (fromInt)
import Util exposing (loadingIfNothing)



-- MODEL


init : Key -> Int -> ( Model, Cmd Msg )
init navKey userId =
    ( Model navKey userId Nothing Nothing, Cmd.batch [ Cmd.map UsersMsg (getUser userId), Cmd.map ClassroomsMsg (getUserClassrooms userId) ] )


type alias Model =
    { navKey : Key, userId : Int, userData : Maybe User, classroomData : Maybe Classrooms }



-- VIEW


view : Model -> Document Msg
view { userData, classroomData } =
    { title = "Classrooms"
    , body =
        [ loadingIfNothing userData
            (\user ->
                p []
                    [ text ("Classrooms of " ++ user.userName ++ ":")
                    , loadingIfNothing classroomData
                        (\classrooms ->
                            ul []
                                (map
                                    (\{ id, name } ->
                                        li
                                            []
                                            [ a [ href ("/users/" ++ fromInt user.id ++ "/classrooms/" ++ fromInt id) ] [ text name ] ]
                                    )
                                    classrooms
                                )
                        )
                    , case user.userType of
                        Teacher ->
                            button [ onClick AddClicked ] [ text "Add classroom" ]

                        _ ->
                            div [] []
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
                Ok users ->
                    case users of
                        [ user ] ->
                            ( { model | userData = Just user }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        UsersMsg _ ->
            ( model, Cmd.none )

        ClassroomsMsg (ApiClient.Classrooms.DataReceived result) ->
            case result of
                Ok classrooms ->
                    ( { model | classroomData = Just classrooms }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ClassroomsMsg _ ->
            ( model, Cmd.none )

        AddClicked ->
            ( model, pushUrl model.navKey ("/users/" ++ fromInt model.userId ++ "/classrooms/add") )


type Msg
    = AddClicked
    | UsersMsg ApiClient.Users.Msg
    | ClassroomsMsg ApiClient.Classrooms.Msg
