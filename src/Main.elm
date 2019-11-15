module Main exposing (main)

import Browser
import Html exposing (text)


type alias Screen =
    { w : Float, h : Float }


type alias Flags =
    {}


type alias Model =
    {}


init : Flags -> ( Model, Cmd msg )
init flags =
    ( {}, Cmd.none )


type Msg
    = NoOp


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


view _ =
    text "hi"
