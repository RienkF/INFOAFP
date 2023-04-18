-- Main module, took a lot of inspiration from https://github.com/rtfeldman/elm-spa-example/blob/master/src/Main.elm


module Main exposing (main, useInit)

import Browser exposing (Document, application)
import Browser.Navigation exposing (Key)
import Html exposing (div)
import Html.Attributes exposing (style)
import Pages.Classrooms exposing (Model, Msg)
import Pages.Login exposing (Model, Msg(..), init)
import Pages.Register
import Platform.Cmd
import Platform.Sub
import Route exposing (Route(..))
import RouteEvent exposing (RouteEvent(..))
import Url exposing (Url)



-- MODEL


type Model
    = LoginModel Pages.Login.Model
    | RegisterModel Pages.Register.Model
    | ClassroomsModel Pages.Classrooms.Model


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ _ key =
    useInit
        (Pages.Login.init key)
        LoginModel
        LoginMsg


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
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | LoginMsg Pages.Login.Msg
    | RegisterMsg Pages.Register.Msg
    | ClassroomsMsg Pages.Classrooms.Msg
    | NoMsg


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            Pages.Login.init (getKey model)
                |> updateWith LoginModel LoginMsg model

        Just Route.Login ->
            Pages.Login.init (getKey model)
                |> updateWith LoginModel LoginMsg model

        Just Route.Register ->
            Pages.Register.init (getKey model)
                |> updateWith RegisterModel RegisterMsg model

        Just (Route.Classrooms userId) ->
            Pages.Classrooms.init (getKey model) userId
                |> updateWith ClassroomsModel ClassroomsMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

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


getKey : Model -> Key
getKey model =
    case model of
        LoginModel loginModel ->
            loginModel.navKey

        RegisterModel registerModel ->
            registerModel.navKey

        ClassroomsModel classroomsModel ->
            classroomsModel.navKey


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



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
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
