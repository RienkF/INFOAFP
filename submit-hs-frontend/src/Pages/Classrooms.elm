module Pages.Classrooms exposing (..)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (p, text)
import String exposing (fromInt)



-- MODEL


init : Key -> Int -> ( Model, Cmd Msg )
init navKey userId =
    ( Model navKey userId, Cmd.none )


type alias Model =
    { navKey : Key, userId : Int }



-- VIEW


view : Model -> Document Msg
view { userId } =
    { title = "Classrooms", body = [ p [] [ text ("Welcome user " ++ fromInt userId) ] ] }



-- UPDATE


type Msg
    = NoMsg
