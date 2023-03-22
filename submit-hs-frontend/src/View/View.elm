module View.View exposing (..)
import Html exposing (p)
import Model.Model exposing (Msg, Model)
import Html exposing (Html)
import Html exposing (text)
import Browser exposing (Document)

view : Model -> Document Msg
view model = { title = "hello world" , body = [p [] [text "hello world"]]}