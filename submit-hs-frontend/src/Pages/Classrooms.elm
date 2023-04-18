module Pages.Classrooms exposing (..)

import ApiClient.Users as Users exposing (Msg(..), User, getUser)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (p, text)



-- MODEL


init : Key -> Int -> ( Model, Cmd Msg )
init navKey userId =
    ( Model navKey userId Nothing, Cmd.map UsersMsg (getUser userId) )


type alias Model =
    { navKey : Key, userId : Int, userData : Maybe User }



-- VIEW


view : Model -> Document Msg
view { userData } =
    { title = "Classrooms"
    , body =
        case userData of
            Nothing ->
                [ p [] [ text "loading" ] ]

            Just user ->
                [ p [] [ text ("Classrooms of " ++ user.userName ++ ":") ] ]
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersMsg (DataReceived result) ->
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

        NoMsg ->
            ( model, Cmd.none )


type Msg
    = NoMsg
    | UsersMsg Users.Msg
