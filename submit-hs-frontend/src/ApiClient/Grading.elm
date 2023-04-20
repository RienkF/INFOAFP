module ApiClient.Grading exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, list, map6)
import String exposing (fromInt)


type alias Gradings =
    List Grading


type alias Grading =
    { id : Int, submission: Int, grade : Float, user: Int, timestamp: String, feedback : String}


type Msg
    = ReceivedGradings (Result Http.Error (List Grading))


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
