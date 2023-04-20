module Pages.Grade exposing (..)

import ApiClient.Users exposing (Msg(..), UserType(..), createUser, getUsers)
import Browser exposing (Document)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (none)
import ApiClient.Grading
import ApiClient.Grading exposing (Msg(..))



-- MODEL


type alias Model =
    { navKey : Key, submissionId: Int, reviewerId: Int, grade : String, feedback: String }

init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey reviewerId submissionId =
    ( Model navKey submissionId reviewerId "0.0" "", Cmd.map GradingMsg (ApiClient.Grading.getGradings 0))



-- VIEW


view : Model -> Document Msg
view { grade } =
    { title = "Grade"
    , body =
        [ h1 [] [ text "Grade" ]
        , h2 [] [ text "Current grade" ]
        , input
            [ value grade, onInput UpdateGrade ]
            []
        , br [] []
        , br [] []
        , button
            [ onClick ChangeGrade ]
            [ text "Change grade" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateGrade String
    |  ChangeGrade
    | GradingMsg ApiClient.Grading.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GradingMsg (ApiClient.Grading.ReceivedGradings result) ->
            case result of
                Ok gradings ->
                    case gradings of
                        (grading :: _) -> ({ model | grade = String.fromFloat grading.grade }, none)
                        _ ->
                            ( model, none)

                -- TODO: Handle
                _ ->
                    ( model, none )

        UpdateGrade grade ->
            ( { model | grade = grade }, none )

        ChangeGrade ->
            ( model, Cmd.map GradingMsg (ApiClient.Grading.addGrade model.submissionId model.reviewerId model.grade model.feedback) )
        
        GradingMsg _ -> (model, none)
