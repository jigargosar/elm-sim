module Main exposing (main)

import Browser
import Html exposing (Html, div)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )



-- Update


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        () ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    []
        |> Sub.batch



-- view


view : Model -> Html Msg
view _ =
    div [] []
