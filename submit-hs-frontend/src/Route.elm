module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s)


type Route
    = Login
    | Register
    | Classrooms Int
    | AddClassroom Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Classrooms (s "users" </> int </> s "classrooms")
        , Parser.map AddClassroom (s "users" </> int </> s "classrooms" </> s "add")
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser
