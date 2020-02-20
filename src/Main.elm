module Main exposing (main)

import Browser
import Doc1
import Doc2
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import Maybe.Extra
import OutlineView
import Pivot exposing (Pivot)
import String exposing (fromInt)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view _ =
    div []
        [ Doc2.viewSampleDoc
        , Doc1.viewSampleDoc
        , OutlineView.viewSample
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
