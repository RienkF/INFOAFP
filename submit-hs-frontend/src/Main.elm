-- Main module, took a lot of inspiration from https://github.com/rtfeldman/elm-spa-example/blob/master/src/Main.elm


module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Html
import Pages.Classrooms exposing (Model, Msg)
import Pages.Login exposing (Model, Msg)
import Platform.Cmd
import Platform.Sub
import Url exposing (Url)



-- MODEL


type Model
    = LoginModel Pages.Login.Model
    | ClassroomsModel Pages.Classrooms.Model


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( LoginModel { selectedUserId = Nothing }, Platform.Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        LoginModel loginModel ->
            let
                loginResult =
                    Pages.Login.view loginModel
            in
            { title = loginResult.title, body = List.map (Html.map LoginMsg) loginResult.body }

        ClassroomsModel classroomsModel ->
            let
                classRoomsResult =
                    Pages.Classrooms.view classroomsModel
            in
            { title = classRoomsResult.title, body = List.map (Html.map ClassroomsMsg) classRoomsResult.body }



-- UPDATE


type Msg
    = LoginMsg Pages.Login.Msg
    | ClassroomsMsg Pages.Classrooms.Msg
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoginMsg loginMsg, LoginModel loginModel ) ->
            Pages.Login.update loginMsg loginModel
                |> updateWith LoginModel LoginMsg model

        ( NoMsg, _ ) ->
            ( model, Platform.Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    NoMsg


onUrlChange : Url -> Msg
onUrlChange _ =
    NoMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Platform.Sub.none



-- MAIN


main : Program () Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }
