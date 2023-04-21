module Pages.AddSubmission exposing (..)

import ApiClient.Attempts exposing (Msg(..), submitAttempt)
import ApiClient.Submissions exposing (Msg(..), createSubmission, getUserSubmission)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (none)
import String exposing (fromInt)
import Util exposing (Either(..))



-- MODEL


type alias Model =
    { navKey : Key
    , userId : Int
    , assignmentId : Int
    , fileData : String
    }


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId assignmentId =
    ( Model navKey userId assignmentId "", Cmd.map SubmissionsMsg <| getUserSubmission userId assignmentId )



-- VIEW


view : Model -> Document Msg
view { fileData } =
    { title = "Add submission"
    , body =
        [ h1 [] [ text "Add a submission" ]
        , h2 [] [ text "Input a the file content of the attempt" ]
        , textarea
            [ value fileData, onInput UpdateFile ]
            []
        , br [] []
        , br [] []
        , button
            [ onClick SubmitAttempt ]
            [ text "Submit" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateFile String
    | SubmitAttempt
    | AttemptMsg ApiClient.Attempts.Msg
    | SubmissionsMsg ApiClient.Submissions.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptMsg (AttemptCreated result) ->
            case result of
                Ok (Just _) ->
                    ( model
                    , pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/assignments/" ++ fromInt model.assignmentId
                    )

                _ ->
                    ( model, Cmd.none )

        AttemptMsg _ ->
            ( model, none )

        SubmissionsMsg (SubmissionCreated result) ->
            case result of
                Ok (Just submission) ->
                    ( model, Cmd.map AttemptMsg <| submitAttempt submission.id model.fileData )

                _ ->
                    ( model, none )

        SubmissionsMsg _ ->
            ( model, none )

        UpdateFile fileData ->
            ( { model | fileData = fileData }, none )

        SubmitAttempt ->
            ( model, Cmd.map SubmissionsMsg <| createSubmission model.userId model.assignmentId )
