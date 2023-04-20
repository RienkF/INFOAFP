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
    = ReceivedAttempt (Result Http.Error (List Attempt))
    | ReceivedSubmissionAttempts (Result Http.Error (List Attempt))
    | AttemptCreated (Result Http.Error (Maybe Attempt))



-- | SubmissionCreated (Result Http.Error (Maybe Attempt))


attemptDecoder : Decoder Attempt
attemptDecoder =
    map4 Attempt
        (field "_attemptId" Decode.int)
        (field "_attemptFile" Decode.string)
        (field "_attemptTimestamp" Decode.string)
        (field "_attemptSubmission" Decode.int)


getAttempt : Int -> Cmd Msg
getAttempt attemptId =
    Http.get
        { url = "http://localhost:3000/attempts?attemptIds=" ++ fromInt attemptId
        , expect = Http.expectJson ReceivedAttempt (list attemptDecoder)
        }


getSubmissionAttempts : Int -> Cmd Msg
getSubmissionAttempts submissionId =
    Http.get
        { url = "http://localhost:3000/attempts?submissionIds=" ++ fromInt submissionId
        , expect = Http.expectJson ReceivedSubmissionAttempts (list attemptDecoder)
        }


submitAttempt : Int -> String -> Cmd Msg
submitAttempt submissionId fileContent =
    Http.post
        { body = jsonBody (encodeAttemptBody submissionId fileContent)
        , expect = Http.expectJson AttemptCreated (maybe attemptDecoder)
        , url = "http://localhost:3000/attempts/add"
        }


encodeAttemptBody : Int -> String -> Value
encodeAttemptBody submissionId fileContent =
    object
        [ ( "submissionId", Encode.int submissionId )
        , ( "file", Encode.string fileContent )
        ]
