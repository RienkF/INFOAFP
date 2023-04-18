module Pages.Classrooms exposing (..)

import ApiClient.Classrooms exposing (Classrooms, getUserClassrooms)
import ApiClient.Users exposing (User, getUser)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (a, li, p, text, ul)
import Html.Attributes exposing (href)
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
                            ul [] (map (\{ id, name } -> li [] [ a [ href ("users/" ++ fromInt user.id ++ "/classrooms/" ++ fromInt id) ] [ text name ] ]) classrooms)
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
                Ok users ->
                    case users of
                        [ user ] ->
                            ( { model | userData = Just user }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                -- TODO: Handle
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

                -- TODO: Handle
                Err _ ->
                    ( model, Cmd.none )

        ClassroomsMsg _ ->
            ( model, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )


type Msg
    = NoMsg
    | UsersMsg ApiClient.Users.Msg
    | ClassroomsMsg ApiClient.Classrooms.Msg
