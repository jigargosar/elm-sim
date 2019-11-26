module Gravitron.Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html, text)
import Random exposing (Seed)
import TypedSvg exposing (circle, rect, svg)
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, rx, ry, width, x, y)
import TypedSvg.Types exposing (Fill(..))



-- Model


type alias Flags =
    { now : Int }


sun =
    { x = 0
    , y = 0
    , radius = 20
    }


type alias Model =
    { seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.now }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view _ =
    let
        sw =
            600

        sh =
            600
    in
    svg [ viewBox 0 0 sw sh, width 600 ]
        [ rect [ x 0, y 0, width sw, height sh, fill (Fill Color.black) ] []
        , circle [ cx 50, cy 50, r 50, fill (Fill Color.yellow) ] []
        ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
