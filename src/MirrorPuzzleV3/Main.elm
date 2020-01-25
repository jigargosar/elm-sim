module MirrorPuzzleV3.Main exposing (main)

import Browser
import Html exposing (Html)
import MirrorPuzzleV3.Demos as Demos



-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    ()


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- Update


type Msg
    = NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    Demos.main
