module Gravitron.Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html, text)
import Random exposing (Seed)
import TypedSvg exposing (circle, g, rect, svg)
import TypedSvg.Attributes exposing (fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, rx, ry, width, x, y)
import TypedSvg.Core as TSC
import TypedSvg.Types exposing (Fill(..), Transform(..))



-- Model


type alias Flags =
    { now : Int }


type alias Planet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    }


initPlanet : Planet
initPlanet =
    { x = 200
    , y = 0
    , vx = 0
    , vy = -10
    , radius = 20
    }


type alias Model =
    { seed : Seed
    , planet : Planet
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.now
      , planet = initPlanet
      }
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
            ( updatePlanet model, Cmd.none )


updatePlanet : Model -> Model
updatePlanet model =
    { model | planet = stepVel model.planet |> gravitateToCenter }


gravitateToCenter ({ x, y, vx, vy } as p) =
    let
        p2x =
            0

        p2y =
            0

        dx =
            p2x - x

        dy =
            p2y - y

        angleToP2 =
            atan2 dy dx

        distanceSquareToP2 =
            dx ^ 2 + dy ^ 2

        p2Mass =
            20 * 1000

        gRadius =
            p2Mass / distanceSquareToP2

        gTheta =
            angleToP2

        ( gvx, gvy ) =
            fromPolar ( gRadius, gTheta )
    in
    { p | vx = vx + gvx, vy = vy + gvy }


stepVel ({ x, y, vx, vy } as p) =
    { p | x = x + vx, y = y + vy }



-- View


fillColor : Color.Color -> TSC.Attribute msg
fillColor =
    Fill >> fill


view : Model -> Html Msg
view model =
    let
        sw =
            600

        sh =
            600

        scx =
            sw / 2

        scy =
            sh / 2
    in
    svg [ viewBox 0 0 sw sh, width sw, height sh ]
        [ renderRect 0 0 sw sh [ fillColor Color.black ]
        , g [ transform [ Translate scx scy ] ]
            [ renderPlanet model.planet
            , renderSun
            ]
        ]


renderSun =
    renderCircle 0 0 50 [ fillColor Color.yellow ]


renderPlanet { x, y, radius } =
    renderCircle x y radius [ fillColor Color.blue ]


renderCircle : Float -> Float -> Float -> List (TSC.Attribute msg) -> TSC.Svg msg
renderCircle cxv cyv rv rest =
    circle ([ cx cxv, cy cyv, r rv ] ++ rest) []


renderRect : Float -> Float -> Float -> Float -> List (TSC.Attribute msg) -> TSC.Svg msg
renderRect xv yv wv hv rest =
    rect ([ x xv, y yv, width wv, height hv ] ++ rest) []


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
