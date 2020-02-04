module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import MainSvgCanvas exposing (..)
import Task



-- Model


type alias Model =
    { screenSize : ( Float, Float ) }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            ( 600, 600 )
    in
    ( { screenSize = size }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenSize = ( scene.width, scene.height ) }, Cmd.none )

        OnResize _ _ ->
            ( model, Browser.Dom.getViewport |> Task.perform GotViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnResize
        ]



-- View


view : Model -> Html Msg
view model =
    canvas model.screenSize
        []
        [ rect "dodgerblue" 100 100 []
        ]


empty : Html msg
empty =
    Html.text ""



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
