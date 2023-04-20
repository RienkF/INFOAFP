module Util exposing (..)

import Html exposing (Html, p, text)


type Either a b
    = Left a
    | Right b


isNothing : Maybe a -> Bool
isNothing val =
    case val of
        Nothing ->
            True

        Just _ ->
            False


loadingIfNothing : Maybe a -> (a -> Html msg) -> Html msg
loadingIfNothing maybeData render =
    case maybeData of
        Nothing ->
            p [] [ text "loading" ]

        Just a ->
            render a


findBy : List a -> (a -> b) -> b -> Maybe a
findBy list findF val =
    case list of
        x :: xs ->
            if findF x == val then
                Just x

            else
                findBy xs findF val

        [] ->
            Nothing
