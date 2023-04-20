module ApiClient.Users exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, Value, andThen, fail, field, int, list, map3, maybe, succeed)
import Json.Encode as Encode exposing (object)
import String exposing (fromInt)


type alias Users =
    List User


type alias User =
    { id : Int, userName : String, userType : UserType }


type UserType
    = Teacher
    | Student
    | Ta


userTypeToString : UserType -> String
userTypeToString userType =
    case userType of
        Teacher ->
            "teacher"

        Ta ->
            "ta"

        Student ->
            "student"


stringToUserType : String -> Maybe UserType
stringToUserType userType =
    case userType of
        "teacher" ->
            Just Teacher

        "ta" ->
            Just Ta

        "student" ->
            Just Student

        _ ->
            Nothing


type Msg
    = UsersDataReceived (Result Http.Error (List User))
    | ReviewerDataReceived (Result Http.Error (List User))
    | UserDataReceived (Result Http.Error (List User))
    | ClassroomUsersDataReceived (Result Http.Error (List User))
    | UserCreated (Result Http.Error (Maybe User))


userTypeDecorder : Decoder UserType
userTypeDecorder =
    Decode.string
        |> andThen
            (\str ->
                case str of
                    "student" ->
                        succeed Student

                    "teacher" ->
                        succeed Teacher

                    "ta" ->
                        succeed Ta

                    somethingElse ->
                        fail <| "Unknown user type: " ++ somethingElse
            )


userDecoder : Decoder User
userDecoder =
    map3 User
        (field "_userId" int)
        (field "_userName" Decode.string)
        (field "_userType" userTypeDecorder)


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = "http://localhost:3000/users"
        , expect = Http.expectJson UsersDataReceived (list userDecoder)
        }


getUser : Int -> Cmd Msg
getUser userId =
    Http.get
        { url = "http://localhost:3000/users?userIds=" ++ fromInt userId
        , expect = Http.expectJson UserDataReceived (list userDecoder)
        }


getReviewer : Int -> Cmd Msg
getReviewer reviewerId =
    Http.get
        { url = "http://localhost:3000/users?userIds=" ++ fromInt reviewerId
        , expect = Http.expectJson ReviewerDataReceived (list userDecoder)
        }


getClassroomUsers : Int -> Cmd Msg
getClassroomUsers classroomId =
    Http.get
        { url = "http://localhost:3000/users?classroomIds=" ++ fromInt classroomId
        , expect = Http.expectJson ClassroomUsersDataReceived (list userDecoder)
        }


createUser : String -> UserType -> Cmd Msg
createUser userName userType =
    Http.post
        { body = jsonBody (encodeUserBody userName userType)
        , expect = Http.expectJson UserCreated (maybe userDecoder)
        , url = "http://localhost:3000/users/add"
        }


encodeUserBody : String -> UserType -> Value
encodeUserBody userName userType =
    object
        [ ( "userName", Encode.string userName )
        , ( "userType", Encode.string (userTypeToString userType) )
        ]
