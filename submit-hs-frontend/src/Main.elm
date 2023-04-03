-- Main module, took a lot of inspiration from https://github.com/rtfeldman/elm-spa-example/blob/master/src/Main.elm


module Main exposing (main, useInit)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Html exposing (div)
import Html.Attributes exposing (style)
import Pages.Classrooms exposing (Model, Msg)
import Pages.Login exposing (Model, Msg(..), init)
import Pages.Register
import Platform.Cmd
import Platform.Sub
import RouteEvent exposing (RouteEvent(..))
import Url exposing (Url)



-- MODEL


type Model
    = LoginModel Pages.Login.Model
    | RegisterModel Pages.Register.Model
    | ClassroomsModel Pages.Classrooms.Model


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    useInit Pages.Login.init LoginModel LoginMsg


useInit : ( a, Cmd b ) -> (a -> Model) -> (b -> Msg) -> ( Model, Cmd Msg )
useInit ( initModel, initCmd ) fModel fCmd =
    ( fModel initModel, Cmd.map fCmd initCmd )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        LoginModel loginModel ->
            useView (Pages.Login.view loginModel) LoginMsg

        RegisterModel registerModel ->
            useView (Pages.Register.view registerModel) RegisterMsg

        ClassroomsModel classroomsModel ->
            useView (Pages.Classrooms.view classroomsModel) ClassroomsMsg


useView : Document a -> (a -> Msg) -> Document Msg
useView result mapMsg =
    { title = result.title, body = [ div [ style "padding-left" "10px" ] (List.map (Html.map mapMsg) result.body) ] }



-- UPDATE


type Msg
    = LoginMsg Pages.Login.Msg
    | RegisterMsg Pages.Register.Msg
    | ClassroomsMsg Pages.Classrooms.Msg
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoginMsg loginMsg, LoginModel loginModel ) ->
            Pages.Login.update loginMsg loginModel
                |> updateWith LoginModel LoginMsg model

        ( RegisterMsg registerMsg, RegisterModel registerModel ) ->
            Pages.Register.update registerMsg registerModel
                |> updateWith RegisterModel RegisterMsg model

        ( NoMsg, _ ) ->
            ( model, Platform.Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg, RouteEvent ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd, routeEvent ) =
    case routeEvent of
        NoEvent ->
            ( toModel subModel
            , Cmd.map toMsg subCmd
            )

        ToClassrooms userId ->
            useInit (Pages.Classrooms.init userId) ClassroomsModel ClassroomsMsg

        ToLogin ->
            useInit Pages.Login.init LoginModel LoginMsg

        ToRegister ->
            useInit Pages.Register.init RegisterModel RegisterMsg


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
