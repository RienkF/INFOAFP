module Main exposing (main)

import Html exposing (text)
import Model.Model exposing (Model, Msg)
import Browser
import Model.Model exposing (init)
import View.View exposing (view)
import Routing.Routing exposing (onUrlRequest)
import Update.Update exposing (update)
import Model.Model exposing (Msg(..))
import Browser exposing (application)
import Subscriptions.Subscriptions exposing (subscriptions)

main : Program () Model Msg
main = application 
  { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = onUrlRequest
    }