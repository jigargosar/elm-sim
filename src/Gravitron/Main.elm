module Gravitron.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
import Task
import TypedSvg as Svg exposing (circle, g, path, rect, svg)
import TypedSvg.Attributes as TSA exposing (d, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, y)
import TypedSvg.Core as TSC
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..))



-- DATA


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


type alias Turret =
    { elapsed : Float
    , rate : Float
    }


initTurret : Turret
initTurret =
    { elapsed = 0
    , rate = 120
    }



-- Model


type alias Flags =
    { now : Int }


type alias Model =
    { seed : Seed
    , planet : Planet
    , turret : Turret
    , ct : Float
    , mouse : Mouse
    , screen : Screen
    }


type alias Screen =
    { w : Float
    , h : Float
    , l : Float
    , r : Float
    , t : Float
    , b : Float
    }


toScreen : Float -> Float -> Screen
toScreen sw sh =
    let
        scx =
            sw / 2

        scy =
            sh / 2
    in
    { w = sw
    , h = sh
    , l = -scx
    , r = scx
    , t = -scy
    , b = scy
    }


type alias Mouse =
    { x : Float
    , y : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.now
      , planet = initPlanet
      , turret = initTurret
      , ct = 0
      , mouse = Mouse 0 0
      , screen = toScreen 600 600
      }
    , Browser.Dom.getViewport
        |> Task.perform OnViewport
    )



-- Update


type Msg
    = Tick Float
    | MouseMoved Float Float
    | OnResize Int Int
    | OnViewport Browser.Dom.Viewport


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    , Browser.Events.onResize OnResize
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( { model | ct = model.ct + 1 }
                |> updatePlanet
                |> updateTurret
            , Cmd.none
            )

        MouseMoved mx my ->
            let
                mouse =
                    model.mouse

                screen =
                    model.screen
            in
            ( { model | mouse = { mouse | x = mx + screen.l, y = my + screen.t } }
            , Cmd.none
            )

        OnViewport { scene } ->
            ( { model | screen = toScreen scene.width scene.height }
            , Cmd.none
            )

        OnResize width height ->
            ( { model | screen = toScreen (toFloat width) (toFloat height) }
            , Cmd.none
            )


updateTurret : Model -> Model
updateTurret model =
    { model
        | turret =
            model.turret
                |> updateTurretElapsed
                |> updateTurretBullet
    }


updateTurretElapsed turret =
    { turret | elapsed = turret.elapsed + 1 }


updateTurretBullet turret =
    if turret.elapsed >= turret.rate then
        { turret | elapsed = 0 }

    else
        turret


updatePlanet : Model -> Model
updatePlanet model =
    let
        mouse =
            model.mouse

        screen =
            model.screen

        sun =
            { x = mouse.x, y = mouse.y, mass = 10 * 1000 }
    in
    { model
        | planet =
            stepVel model.planet
                |> gravitateTo sun
                |> clampVelocity 30
                |> bounceOffScreen screen
    }


clampVelocity n p =
    let
        clampPart =
            clamp -n n
    in
    { p | vx = clampPart p.vx, vy = clampPart p.vy }


bounceOffScreen s =
    let
        bounceX p =
            if p.x < s.l then
                { p | x = s.l, vx = p.vx * -0.8 }

            else if p.x > s.r then
                { p | x = s.r, vx = p.vx * -0.8 }

            else
                p

        bounceY p =
            if p.y < s.t then
                { p | y = s.t, vy = p.vy * -0.8 }

            else if p.y > s.b then
                { p | y = s.b, vy = p.vy * -0.8 }

            else
                p
    in
    bounceX >> bounceY


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
        s =
            model.screen
    in
    svg [ style "position" "fixed", viewBox s.l s.t s.w s.h, width s.w, height s.h ]
        [ renderRect s.l s.t s.w s.h [ fillColor Color.black ]
        , renderPlanet model.planet
        , renderSun model.mouse
        , renderTurret
        , renderTurret2 model.turret
        ]


renderSun { x, y } =
    renderCircle x y 50 [ fillColor Color.yellow ]


renderTurret2 turret =
    let
        ( x, y ) =
            ( -100, 100 )

        r =
            20

        pctCompleted =
            100 / turret.rate * turret.elapsed

        innerR =
            r / 100 * pctCompleted
    in
    g [ transform [ Translate x y ] ]
        [ renderCircle 0 0 r [ fillColor Color.green ]
        , renderCircle 0 0 innerR [ fillColor <| whiteA 0.5 ]
        ]


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


renderTurret =
    let
        maxRadius =
            20

        outerStrokeWidth =
            1.5

        gap =
            outerStrokeWidth * 2

        innerRadius =
            maxRadius - gap

        outerRadius =
            maxRadius
    in
    g [ transform [ Translate -100 -100 ] ]
        [ renderCircle 0 0 innerRadius [ fillColor Color.green ]
        , renderCircle 0
            0
            outerRadius
            [ fillNone
            , stroke <| Color.rgba 1 1 1 0.8

            {- , strokeWidth outerStrokeWidth
               , TSA.strokeLinecap StrokeLinecapRound
               , TSA.strokeLinejoin StrokeLinejoinRound
            -}
            ]
        , renderCircle 0
            0
            outerRadius
            [ fillNone
            , stroke <| Color.orange
            , strokeWidth outerStrokeWidth
            , TSA.strokeLinecap StrokeLinecapRound
            , TSA.strokeLinejoin StrokeLinejoinRound
            ]
        ]


fillNone =
    fill FillNone


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
