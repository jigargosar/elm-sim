module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import IO
import Number2 exposing (Float2, Int2)
import Task



-- Model


type alias Model =
    { browserWH : Float2 }


changeBrowserWH wh m =
    { m | browserWH = wh }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { browserWH = ( 600, 600 ) }
    , IO.getBrowserWH |> Task.perform BrowserResized
    )



-- Update


type Msg
    = NoOp
    | BrowserResized Float2


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        BrowserResized wh ->
            ( changeBrowserWH wh model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ IO.onBrowserWH BrowserResized
        ]



-- View


view : Model -> Html Msg
view _ =
    empty


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