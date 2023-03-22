module Subscriptions.Subscriptions exposing (..)
import Model.Model exposing (Msg)
import Model.Model exposing (Model)
import Platform.Sub exposing (none)

subscriptions : Model -> Sub Msg
subscriptions _ = none