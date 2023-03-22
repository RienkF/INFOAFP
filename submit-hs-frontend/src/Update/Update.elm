module Update.Update exposing (..)
import Model.Model exposing (Msg)
import Model.Model exposing (Model)
import Platform.Cmd exposing (none)

update : Msg -> Model -> ( Model, Cmd Msg )
update _ model = (model, none)