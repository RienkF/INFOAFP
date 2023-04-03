module Util exposing (..)


isNothing : Maybe a -> Bool
isNothing val =
    case val of
        Nothing ->
            True

        Just _ ->
            False
