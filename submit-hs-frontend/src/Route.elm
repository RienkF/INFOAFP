module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s)


type Route
    = Login
    | Register
    | Classrooms Int
    | Classroom Int Int
    | AddClassroom Int
    | AddParticipant Int Int
    | AddAssignment Int Int
    | Assignment Int Int
    | AddSubmission Int Int
    | AddAttempt Int Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Classrooms (s "users" </> int </> s "classrooms")
        , Parser.map Classroom (s "users" </> int </> s "classrooms" </> int)
        , Parser.map AddClassroom (s "users" </> int </> s "classrooms" </> s "add")
        , Parser.map AddParticipant (s "users" </> int </> s "classrooms" </> int </> s "addParticipant")
        , Parser.map AddAssignment (s "users" </> int </> s "classrooms" </> int </> s "assignments" </> s "add")
        , Parser.map Assignment (s "users" </> int </> s "assignments" </> int)
        , Parser.map AddSubmission (s "users" </> int </> s "assignments" </> int </> s "addSubmission")
        , Parser.map AddAttempt (s "users" </> int </> s "assignments" </> int </> s "addAttempt")
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser
