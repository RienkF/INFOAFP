module ApiClient.Attempts exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, field, list, map4, maybe)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Attempts =
    List Attempt


type alias Attempt =
    { id : Int, file : String, timeStamp : String, submissionId : Int }


type Msg
    = ReceivedSubmissionAttempts (Result Http.Error (List Attempt))



-- | SubmissionCreated (Result Http.Error (Maybe Attempt))


attemptDecoder : Decoder Attempt
attemptDecoder =
    map4 Attempt
        (field "_attemptId" Decode.int)
        (field "_attemptFile" Decode.string)
        (field "_attemptTimestamp" Decode.string)
        (field "_attemptSubmission" Decode.int)


getSubmissionAttempts : Int -> Cmd Msg
getSubmissionAttempts submissionId =
    Http.get
        { url = "http://localhost:3000/attempts?submissionIds=" ++ fromInt submissionId
        , expect = Http.expectJson ReceivedSubmissionAttempts (list attemptDecoder)
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
