module Pages.Grade exposing (..)

import ApiClient.Users exposing (Msg(..), UserType(..))
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (none)
import ApiClient.Grading
import ApiClient.Grading exposing (Msg(..))
import ApiClient.Submissions
import ApiClient.Attempts
import ApiClient.Attempts exposing (Attempt)
import Util exposing (loadingIfNothing)
import Json.Decode exposing (at)
import Util exposing (Either(..))



-- MODEL


type alias Model =
    { navKey : Key, submissionId: Int, reviewerId: Int, grade : String, feedback: String, attempt: Maybe (Either Attempt Bool) }

init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey reviewerId submissionId =
    ( Model navKey submissionId reviewerId "0.0" "No feedback" Nothing, Cmd.map AttemptsMessage (ApiClient.Attempts.getSubmissionAttempts submissionId))

-- VIEW


view : Model -> Document Msg
view { grade, feedback, attempt} =
    { title = "Grade"
    , body =
        [ h1 [] [ text "Grade" ]
        , loadingIfNothing attempt <|
            \attemptMaybe ->
                case attemptMaybe of
                    Left submission ->
                        div []
                            [ h2 [] [ text "File"]
                            , p [] [ text submission.file ]
                            , h2 [] [ text "Set grade" ]
                            , input
                                [ value grade, onInput UpdateGrade ]
                                []
                            , br [] []
                            , br [] []
                            , input
                                [ value feedback, onInput UpdateFeedback ]
                                []
                            , br [] []
                            , br [] []
                            , button
                                [ onClick ChangeGrade ]
                                [ text "Change grade" ]
                            ]

                    Right _ ->
                        p [] [ text "error: no submission for this assignment found" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateGrade String
    | UpdateFeedback String
    | ChangeGrade
    | NewGrade ApiClient.Grading.Msg
    | AttemptsMessage ApiClient.Attempts.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttemptsMessage (ApiClient.Attempts.ReceivedSubmissionAttempts result) ->
            case result of
                Ok attempts ->
                    case attempts of
                        ( attempt :: _  ) -> ({ model | attempt = Just (Left attempt)}, none)
                        _ -> ({ model | attempt = Just (Right False)}, none)

                -- TODO: Handle
                _ ->
                    ( model, none )

        UpdateGrade grade ->
            ( { model | grade = grade }, none )
        
        UpdateFeedback feedback ->
            ( { model | feedback = feedback }, none )

        ChangeGrade ->
            ( model, Cmd.map NewGrade (ApiClient.Grading.addGrade model.submissionId model.reviewerId model.grade model.feedback) )
        
        _ -> (model, none)
