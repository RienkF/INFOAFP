module ApiClient.Classrooms exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, field, int, list, map2, maybe)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Classrooms =
    List Classroom


type alias Classroom =
    { id : Int, name : String }


type alias ClassroomParticipant =
    { userId : Int, classroomId : Int }


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Classroom))
    | ClassroomCreated (Result Http.Error (Maybe Classroom))
    | ClassroomParticipantAdded (Result Http.Error (Maybe ClassroomParticipant))


classroomDecoder : Decoder Classroom
classroomDecoder =
    map2 Classroom
        (field "_classroomId" int)
        (field "_classroomName" Decode.string)


classroomParticipantDecoder : Decoder ClassroomParticipant
classroomParticipantDecoder =
    map2 ClassroomParticipant
        (field "_classroomParticipantUser" int)
        (field "_classroomParticipantClassroom" int)


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


addClassroomParticipant : Int -> Int -> Cmd Msg
addClassroomParticipant classroomId userId =
    Http.post
        { body = jsonBody (encodeClassroomParticipantBody classroomId userId)
        , expect = Http.expectJson ClassroomParticipantAdded (maybe classroomParticipantDecoder)
        , url = "http://localhost:3000/classroomParticipants/add"
        }


encodeClassroomParticipantBody : Int -> Int -> Value
encodeClassroomParticipantBody classroomId userId =
    object
        [ ( "classroomId", Encode.int classroomId )
        , ( "userId", Encode.int userId )
        ]
