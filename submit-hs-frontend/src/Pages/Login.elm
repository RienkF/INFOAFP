module Pages.Login exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (none)
import String exposing (fromInt)



-- MODEL


type alias Model =
    { selectedUserId : Maybe Int }



-- VIEW


view : Model -> Document Msg
view { selectedUserId } =
    let
        selectedValue =
            case selectedUserId of
                Just id ->
                    fromInt id

                Nothing ->
                    ""
    in
    { title = "Login"
    , body =
        [ select
            [ value selectedValue ]
            -- TODO: Load options here
            [ option [ value "1" ] [ text "Rienk" ]
            , option [ value "2" ] [ text "Martin" ]
            , option [ value "3" ] [ text "Gijs" ]
            ]
        , button
            [ onClick SubmitUser ]
            [ text "Ok" ]
        ]
    }



-- UPDATE


type Msg
    = SubmitUser


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    -- TODO: Handle
    ( model, none )
