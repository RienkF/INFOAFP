module Util exposing (..)

import Html exposing (Html, p, text)


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
