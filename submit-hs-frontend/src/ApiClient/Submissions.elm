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


createSubmission : Int -> Int -> Cmd Msg
createSubmission userId assignmentId =
    Http.post
        { body = jsonBody (encodeSubmissionBody userId assignmentId)
        , expect = Http.expectJson SubmissionCreated (maybe submssionDecoder)
        , url = "http://localhost:3000/submissions/add"
        }


encodeSubmissionBody : Int -> Int -> Value
encodeSubmissionBody userId assignmentId =
    object
        [ ( "userId", Encode.int userId )
        , ( "assignmentId", Encode.int assignmentId )
        ]
