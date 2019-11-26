module Gravitron.Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Json.Decode as JD
import Random exposing (Seed)
import TypedSvg exposing (circle, g, rect, svg)
import TypedSvg.Attributes exposing (fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
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
    let
        ( vx, vy ) =
            fromPolar ( 10, degrees -90 )

        ( x, y ) =
            fromPolar ( 200, degrees 0 )
    in
    { x = x
    , y = y
    , vx = vx
    , vy = vy
    , radius = 20
    }


type alias Model =
    { seed : Seed
    , planet : Planet
    , ct : Int
    , mouse : Mouse
    }


type alias Mouse =
    { x : Float
    , y : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.now
      , planet = initPlanet
      , ct = 0
      , mouse = Mouse 0 0
      }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float
    | MouseMoved Float Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( { model | ct = model.ct + 1 }
                |> updatePlanet
            , Cmd.none
            )

        MouseMoved mx my ->
            let
                mouse =
                    model.mouse
            in
            ( { model | mouse = { mouse | x = mx, y = my } }
                |> updatePlanet
            , Cmd.none
            )


updatePlanet : Model -> Model
updatePlanet model =
    let
        sun =
            { x = 0, y = 0, mass = 20 * 1000 }
    in
    { model | planet = stepVel model.planet |> gravitateTo sun }


gravitateTo p2 p1 =
    let
        ( gvx, gvy ) =
            gravityVectorTo p2 p1
    in
    accelerate gvx gvy p1


stepVel ({ x, y, vx, vy } as p) =
    { p | x = x + vx, y = y + vy }


accelerate ax ay ({ vx, vy } as p) =
    { p | vx = vx + ax, vy = vy + ay }


gravityVectorTo p2 p1 =
    let
        p2x =
            p2.x

        p2y =
            p2.y

        p1x =
            p1.x

        p1y =
            p1.y

        p2Mass =
            p2.mass

        dx =
            p2x - p1x

        dy =
            p2y - p1y

        angleToP2 =
            atan2 dy dx

        distanceSquareToP2 =
            dx ^ 2 + dy ^ 2

        gRadius =
            p2Mass / distanceSquareToP2

        gTheta =
            angleToP2
    in
    fromPolar ( gRadius, gTheta )



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
