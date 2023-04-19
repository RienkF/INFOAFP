module ApiClient.Assignments exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, field, list, map6, maybe)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Assignments =
    List Assignment


type alias Assignment =
    { id : Int, description : String, startDate : String, deadline : String, weight : Float, classroomId : Int }


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Assignment))
    | AssignmentCreated (Result Http.Error (Maybe Assignment))


assignmentDecoder : Decoder Assignment
assignmentDecoder =
    map6 Assignment
        (field "_assignmentId" Decode.int)
        (field "_assignmentDescription" Decode.string)
        (field "_assignmentStartDate" Decode.string)
        (field "_assignmentDeadline" Decode.string)
        (field "_assignmentWeight" Decode.float)
        (field "_assignmentClassroom" Decode.int)


getClassroomAssignments : Int -> Cmd Msg
getClassroomAssignments classroomId =
    Http.get
        { url = "http://localhost:3000/assignments?classroomIds=" ++ fromInt classroomId
        , expect = Http.expectJson DataReceived (list assignmentDecoder)
        }


createAssignment : String -> String -> String -> Float -> Int -> Cmd Msg
createAssignment description startDate deadline weight classroomId =
    Http.post
        { body = jsonBody (encodeAssignmentBody description startDate deadline weight classroomId)
        , expect = Http.expectJson AssignmentCreated (maybe assignmentDecoder)
        , url = "http://localhost:3000/assignments/add"
        }


encodeAssignmentBody : String -> String -> String -> Float -> Int -> Value
encodeAssignmentBody description startDate deadline weight classroomId =
    object
        [ ( "assignmentDescription", Encode.string description )
        , ( "assignmentStartDate", Encode.string startDate )
        , ( "assignmentDeadline", Encode.string deadline )
        , ( "assignmentWeight", Encode.float weight )
        , ( "assignmentClassroom", Encode.int classroomId )
        ]
