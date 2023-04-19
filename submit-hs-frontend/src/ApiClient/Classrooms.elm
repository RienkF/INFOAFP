module ApiClient.Classrooms exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, field, int, list, map2, maybe)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Classrooms =
    List Classroom


type alias Classroom =
    { id : Int, name : String }


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Classroom))
    | ClassroomCreated (Result Http.Error (Maybe Classroom))


classroomDecoder : Decoder Classroom
classroomDecoder =
    map2 Classroom
        (field "_classroomId" int)
        (field "_classroomName" Decode.string)


getClassroom : Int -> Cmd Msg
getClassroom classroomId =
    Http.get
        { url = "http://localhost:3000/classrooms?classroomIds=" ++ fromInt classroomId
        , expect = Http.expectJson DataReceived (list classroomDecoder)
        }


getUserClassrooms : Int -> Cmd Msg
getUserClassrooms userId =
    Http.get
        { url = "http://localhost:3000/classrooms?userIds=" ++ fromInt userId
        , expect = Http.expectJson DataReceived (list classroomDecoder)
        }


createClassroom : String -> Int -> Cmd Msg
createClassroom classname teacherId =
    Http.post
        { body = jsonBody (encodeClassroomBody classname teacherId)
        , expect = Http.expectJson ClassroomCreated (maybe classroomDecoder)
        , url = "http://localhost:3000/classrooms/add"
        }


encodeClassroomBody : String -> Int -> Value
encodeClassroomBody className teacherId =
    object
        [ ( "classroomName", Encode.string className )
        , ( "teacherId", Encode.int teacherId )
        ]
