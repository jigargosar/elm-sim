module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Html exposing (text)
import Task


type alias Screen =
    { w : Float, h : Float }


type alias Flags =
    {}


type alias Model =
    { screen : Screen }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screen = Screen 600 400
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )


type Msg
    = NoOp
    | GotViewport Viewport


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screen = Screen scene.width scene.height }, Cmd.none )


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


view _ =
    text "hi"
