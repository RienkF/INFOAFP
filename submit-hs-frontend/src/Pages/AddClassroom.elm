module Pages.AddClassroom exposing (..)

import ApiClient.Classrooms exposing (Msg(..), createClassroom)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.Classrooms exposing (Msg(..))
import Platform.Cmd exposing (none)
import String exposing (fromInt)



-- MODEL


type alias Model =
    { navKey : Key, userId : Int, className : String }


init : Key -> Int -> ( Model, Cmd Msg )
init navKey userId =
    ( Model navKey userId "", Cmd.none )



-- VIEW


view : Model -> Document Msg
view { className } =
    { title = "Add classroom"
    , body =
        [ h1 [] [ text "Add a classroom" ]
        , h2 [] [ text "Input name of the class" ]
        , input
            [ value className, onInput UpdateClassName ]
            []
        , br [] []
        , br [] []
        , button
            [ onClick CreateClassroom ]
            [ text "Create classroom" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateClassName String
    | CreateClassroom
    | ClassroomMsg ApiClient.Classrooms.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClassroomMsg (ClassroomCreated result) ->
            case result of
                Ok (Just res) ->
                    ( model
                    , pushUrl model.navKey ("/users/" ++ fromInt model.userId ++ "/classrooms/" ++ fromInt res.id)
                    )

                -- TODO: Handle
                _ ->
                    ( model, Cmd.none )

        ClassroomMsg _ ->
            ( model, none )

        UpdateClassName classname ->
            ( { model | className = classname }, none )

        CreateClassroom ->
            ( model, Cmd.map ClassroomMsg <| createClassroom model.className model.userId )
