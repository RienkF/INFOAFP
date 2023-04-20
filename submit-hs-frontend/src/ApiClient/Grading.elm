module ApiClient.Grading exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, field, list, map6, maybe)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Gradings =
    List Grading


type alias Grading =
    { id : Int, submission: Int, grade : Float, user: Int, timestamp: String, feedback : String}


type Msg
    = ReceivedGradings (Result Http.Error (List Grading))
    | GradingCreated (Result Http.Error (Maybe Grading))


gradingDecoder : Decoder Grading
gradingDecoder =
    map6 Grading
        (field "_gradingId" Decode.int)
        (field "_gradingSubmission" Decode.int)
        (field "_gradingGrade" Decode.float)
        (field "_gradingUser" Decode.int)
        (field "_gradingTimestamp" Decode.string)
        (field "_gradingFeedback" Decode.string)


getGradings : Int -> Cmd Msg
getGradings submissionId =
    Http.get
        { url = "http://localhost:3000/gradings?submissionIds=" ++ fromInt submissionId
        , expect = Http.expectJson ReceivedGradings (list gradingDecoder)
        }

addGrade : Int -> Int -> String -> String -> Cmd Msg
addGrade submissionId reviewerId grade feedback =
    Http.post
        { body = jsonBody (encodeGradeBody submissionId reviewerId grade feedback)
        , expect = Http.expectJson GradingCreated (maybe gradingDecoder)
        , url = "http://localhost:3000/gradings/add"
        }

encodeGradeBody : Int -> Int -> String -> String -> Value
encodeGradeBody submissionId reviewerid grade feedback =
    object
        [ ( "submissionId", Encode.int submissionId )
        , ( "reviewerId", Encode.int reviewerid )
        , ( "grade", Encode.float (String.toFloat grade |> Maybe.withDefault 0) )
        , ( "feedback", Encode.string feedback )
        ]
