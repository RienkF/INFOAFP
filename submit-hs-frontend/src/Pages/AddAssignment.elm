module Pages.AddAssignment exposing (..)

import ApiClient.Assignments exposing (Msg(..), createAssignment)
import ApiClient.Classrooms exposing (Msg(..))
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
    { navKey : Key, userId : Int, classroomid : Int, description : String, startDate : String, deadline : String, weight : Float }


init : Key -> Int -> Int -> ( Model, Cmd Msg )
init navKey userId classroomId =
    ( Model navKey userId classroomId "" "" "" 0, Cmd.none )



-- VIEW


view : Model -> Document Msg
view { description, startDate, deadline, weight } =
    { title = "Add assignment"
    , body =
        [ h1 [] [ text "Add an assignment" ]
        , h2 [] [ text "Input a description of the assignment" ]
        , input
            [ value description, onInput UpdateDescription ]
            []
        , h2 [] [ text "Input a start date for the assignment" ]
        , input
            [ type_ "datetime-local", value startDate, onInput UpdateStartDate ]
            []
        , h2 [] [ text "Input a deadline for the assignment" ]
        , input
            [ type_ "datetime-local", value deadline, onInput UpdateDeadline ]
            []
        , h2 [] [ text "Input the weight of the assignment" ]
        , input
            [ type_ "number"
            , step "0.01"
            , value <| fromFloat weight
            , onInput
                (\input ->
                    let
                        floatMaybe =
                            String.toFloat input
                    in
                    UpdateWeight <|
                        case floatMaybe of
                            Just val ->
                                val

                            Nothing ->
                                weight
                )
            ]
            []
        , br [] []
        , br [] []
        , button
            [ onClick CreateAssignment ]
            [ text "Create assignment" ]
        ]
    }



-- UPDATE


type Msg
    = UpdateDescription String
    | UpdateStartDate String
    | UpdateDeadline String
    | UpdateWeight Float
    | CreateAssignment
    | AssignmentsMsg ApiClient.Assignments.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AssignmentsMsg (AssignmentCreated result) ->
            case result of
                Ok (Just _) ->
                    ( model
                    , pushUrl model.navKey <| "/users/" ++ fromInt model.userId ++ "/classrooms/" ++ fromInt model.classroomid
                    )

                _ ->
                    ( model, Cmd.none )

        AssignmentsMsg _ ->
            ( model, none )

        UpdateDescription description ->
            ( { model | description = description }, none )

        UpdateStartDate startDate ->
            ( { model | startDate = startDate }, none )

        UpdateDeadline deadline ->
            ( { model | deadline = deadline }, none )

        UpdateWeight weight ->
            ( { model | weight = weight }, none )

        CreateAssignment ->
            ( model, Cmd.map AssignmentsMsg <| createAssignment model.description model.startDate model.deadline model.weight model.classroomid )
