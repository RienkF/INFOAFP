module Pages.Classrooms exposing (..)

import Browser exposing (Document)



-- MODEL


type alias Model =
    { userId : Int }



-- VIEW


view : Model -> Document Msg
view { userId } =
    { title = "classroom", body = [] }



-- UPDATE


type Msg
    = NoMsg
