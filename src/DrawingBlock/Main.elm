module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Html exposing (Html)
import Number2 as NT exposing (Float2, Int2)
import Task



-- Model


type alias Model =
    { browserWH : Float2 }


setBrowserWH wh m =
    { m | browserWH = wh }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { browserWH = ( 600, 600 ) }
    , BD.getViewport |> Task.map (.scene >> whFromRecord) |> Task.perform BrowserResized
    )


whFromRecord : { a | width : b, height : c } -> ( b, c )
whFromRecord r =
    ( r.width, r.height )



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
            ( setBrowserWH wh model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BE.onResize (\w h -> BrowserResized (NT.toFloat ( w, h )))
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
