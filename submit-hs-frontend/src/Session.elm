module Session exposing (..)

import Browser.Navigation exposing (Key)



-- TYPES


type alias UserId =
    Int


type Session
    = LoggedIn Key UserId
    | Guest Key
