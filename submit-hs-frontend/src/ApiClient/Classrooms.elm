module ApiClient.Classrooms exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, map2)
import String exposing (fromInt)


type alias Classrooms =
    List Classroom


type alias Classroom =
    { id : Int, name : String }


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Classroom))


classroomDecoder : Decoder Classroom
classroomDecoder =
    map2 Classroom
        (field "_classroomId" int)
        (field "_classroomName" Decode.string)


getUserClassrooms : Int -> Cmd Msg
getUserClassrooms userId =
    Http.get
        { url = "http://localhost:3000/classrooms?userIds=" ++ fromInt userId
        , expect = Http.expectJson DataReceived (list classroomDecoder)
        }
