module ApiClient.Users exposing (..)

import Http
import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map3, string, succeed)



-- MODEL


type Model
    = Loading
    | Users Users


type alias Users =
    List User


type alias User =
    { id : Int, userName : String, userType : UserType }


type UserType
    = Teacher
    | Student
    | Ta


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List User))



-- UPDATE


url : String
url =
    "http://localhost:3000/users"


userTypeDecorder : Decoder UserType
userTypeDecorder =
    string
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
        (field "_userName" string)
        (field "_userType" userTypeDecorder)


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived (list userDecoder)
        }
