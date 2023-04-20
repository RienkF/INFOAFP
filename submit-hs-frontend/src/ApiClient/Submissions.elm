module ApiClient.Submissions exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, field, list, map3, maybe)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Submissions =
    List Submission


type alias Submission =
    { id : Int, userId : Int, assignmentId : Int }


type Msg
    = ReceivedSubmissions (Result Http.Error (List Submission))
    | ReceivedUserSubmissions (Result Http.Error (List Submission))
    | ReceivedAssignmentSubmissions (Result Http.Error (List Submission))
    | SubmissionCreated (Result Http.Error (Maybe Submission))


submssionDecoder : Decoder Submission
submssionDecoder =
    map3 Submission
        (field "_submissionId" Decode.int)
        (field "_submissionUser" Decode.int)
        (field "_submissionAssignment" Decode.int)


getUserSubmission : Int -> Int -> Cmd Msg
getUserSubmission userId assignmentId =
    Http.get
        { url = "http://localhost:3000/submissions?userIds=" ++ fromInt userId ++ "&assignmentIds=" ++ fromInt assignmentId
        , expect = Http.expectJson ReceivedUserSubmissions (list submssionDecoder)
        }


getAssignmentSubmissions : Int -> Cmd Msg
getAssignmentSubmissions assignmentId =
    Http.get
        { url = "http://localhost:3000/submissions?assignmentIds=" ++ fromInt assignmentId
        , expect = Http.expectJson ReceivedAssignmentSubmissions (list submssionDecoder)
        }



-- createAssignment : String -> String -> String -> Float -> Int -> Cmd Msg
-- createAssignment description startDate deadline weight classroomId =
--     Http.post
--         { body = jsonBody (encodeAssignmentBody description startDate deadline weight classroomId)
--         , expect = Http.expectJson AssignmentCreated (maybe assignmentDecoder)
--         , url = "http://localhost:3000/assignments/add"
--         }
-- encodeAssignmentBody : String -> String -> String -> Float -> Int -> Value
-- encodeAssignmentBody description startDate deadline weight classroomId =
--     object
--         [ ( "assignmentDescription", Encode.string description )
--         , ( "assignmentStartDate", Encode.string startDate )
--         , ( "assignmentDeadline", Encode.string deadline )
--         , ( "assignmentWeight", Encode.float weight )
--         , ( "assignmentClassroom", Encode.int classroomId )
--         ]
