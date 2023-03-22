module Model.Model exposing (..)
import Browser
import Url
import Url exposing (Url)
import Browser.Navigation exposing (Key)
import Platform.Cmd exposing (none)

type alias Model = { userId : Maybe Int }

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | NoMsg

init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key = ( { userId = Nothing }, none )