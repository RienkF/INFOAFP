module Pages.Classrooms exposing (..)

import Browser exposing (Document)
import Html exposing (p, text)
import String exposing (fromInt)



-- MODEL


init : Int -> ( Model, Cmd Msg )
init userId =
    ( Model userId, Cmd.none )


type alias Model =
    { userId : Int }



-- VIEW


view : Model -> Document Msg
view { userId } =
    { title = "Classrooms", body = [ p [] [ text ("Welcome user " ++ fromInt userId) ] ] }



-- UPDATE


type Msg
    = NoMsg
