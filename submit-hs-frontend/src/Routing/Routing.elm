module Routing.Routing exposing (..)
import Browser exposing (UrlRequest)
import Model.Model exposing (Msg)
import Model.Model exposing (Msg(..))
import Url exposing (Url)

onUrlRequest : UrlRequest -> Msg
onUrlRequest req = NoMsg

onUrlChange : Url -> Msg
onUrlChange url = NoMsg