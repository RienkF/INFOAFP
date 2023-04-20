module Pages.AddAttempt exposing (..)

import ApiClient.Assignments exposing (Msg(..), createAssignment)
import ApiClient.Attempts exposing (Msg(..), submitAttempt)
import ApiClient.Classrooms exposing (Msg(..))
import ApiClient.Submissions exposing (Submission)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.Classrooms exposing (Msg(..))
import Platform.Cmd exposing (none)
import String exposing (fromFloat, fromInt)



-- MODEL


type alias Model =
    { navKey : Key
    , userId : Int
    , assignmentId : Int
    , submissionData : Maybe Submission
    , fileData : String
    }


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId assignmentId =
    ( Model navKey userId assignmentId Nothing "", Cmd.none )



-- VIEW


view : Model -> Document Msg
view { fileData } =
    { title = "Add attempt"
    , body =
        [ h1 [] [ text "Add an attempt" ]

        -- TODO:  First load the submission and error if it does not exist
        , h2 [] [ text "Input a the file content of the attempt" ]
        , input
            [ value fileData, onInput UpdateFile ]
            []
        , br [] []
        , br [] []
        , button
            [ onClick SubmitAttempt ]
            [ text "Submit attempt" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateFile String
    | SubmitAttempt
    | AttemptMsg ApiClient.Attempts.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptMsg (AttemptCreated result) ->
            case result of
                Ok (Just _) ->
                    ( model
                    , pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId
                    )

                -- TODO: Handle
                _ ->
                    ( model, Cmd.none )

        AttemptMsg _ ->
            ( model, none )

        UpdateFile fileData ->
            ( { model | fileData = fileData }, none )

        SubmitAttempt ->
            case model.submissionData of
                Just submission ->
                    ( model, Cmd.map AttemptMsg <| submitAttempt submission.id model.fileData )

                -- Can't submit if there is no submission
                Nothing ->
                    ( model, none )
