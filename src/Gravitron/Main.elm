module Gravitron.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
import Task
import TypedSvg exposing (circle, g, rect, svg)
import TypedSvg.Attributes exposing (fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core as TSC
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..))



-- Constants


turretFireRateInTicks =
    60


bulletInitialSpeed =
    5


bulletMaxSpeed =
    11


initialSunMass =
    1500


bulletWallDrag =
    0.8


bulletUpdateDrag =
    0.999



-- DATA


type alias Sun =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    , mass : Float
    }


initSun : Sun
initSun =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , radius = 20
    , mass = initialSunMass
    }


type alias Turret =
    { x : Float
    , y : Float
    , radius : Float
    , color : Color.Color
    }


initTurretAt : Float -> Float -> Turret
initTurretAt x y =
    { x = x
    , y = y
    , radius = 20
    , color = Color.lightGreen
    }


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    }


initBullet : Float -> Float -> Float -> Float -> Bullet
initBullet x y speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )
    in
    { x = x
    , y = y
    , vx = vx
    , vy = vy
    , radius = 5
    }



-- Model


type alias Flags =
    { now : Int }


type alias Model =
    { seed : Seed
    , turret : Turret
    , ticksSinceLastFire : Float
    , fireRateInTicks : Float
    , bullets : List Bullet
    , sun : Sun
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
      , sun = initSun
      , turret = initTurretAt 100 -100
      , ticksSinceLastFire = 0
      , fireRateInTicks = turretFireRateInTicks
      , bullets = []
      , mouse = Mouse 0 0
      , screen = toScreen 600 600
      }
    , Browser.Dom.getViewport |> Task.perform OnViewport
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
            ( updateOnTick model
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


updateOnTick : Model -> Model
updateOnTick ({ screen, sun, bullets } as model) =
    let
        newSun =
            model.sun
                |> stepVel
                |> followXY model.mouse

        ( shouldFireBullet, newTicksSinceFire ) =
            turretStepTriggerAndFireBulletIfReady model

        appendNewBulletIfFired =
            shouldFireBullet
                |> Maybe.map (\_ -> (::) (initBullet 0 0 bulletInitialSpeed (degrees 180)))
                |> Maybe.withDefault identity

        newBullets =
            let
                updateBullet bullet =
                    bullet
                        |> stepVel
                        |> gravitateTo sun
                        |> applyDrag bulletUpdateDrag
                        |> clampVelocity bulletMaxSpeed
                        |> bounceOffScreen screen
            in
            bullets
                |> appendNewBulletIfFired
                |> List.map updateBullet
    in
    { model
        | sun = newSun
        , ticksSinceLastFire = newTicksSinceFire
        , bullets = newBullets
    }


type FireBullet
    = FireBullet


turretStepTriggerAndFireBulletIfReady :
    { a | ticksSinceLastFire : Float, fireRateInTicks : Float }
    -> ( Maybe FireBullet, Float )
turretStepTriggerAndFireBulletIfReady { ticksSinceLastFire, fireRateInTicks } =
    let
        newTicksSinceLastFire =
            ticksSinceLastFire + 1
    in
    if newTicksSinceLastFire >= fireRateInTicks then
        ( Just FireBullet, 0 )

    else
        ( Nothing, newTicksSinceLastFire )


applyDrag drag p =
    { p | vx = p.vx * drag, vy = p.vy * drag }


followXY { x, y } sun =
    let
        dx =
            x - sun.x

        dy =
            y - sun.y

        ( rad, theta ) =
            toPolar ( dx, dy )

        ( nvx, nvy ) =
            fromPolar ( rad / 15, theta )
    in
    { sun | vx = nvx, vy = nvy }



{-
   updatePlanet : Model -> Model
   updatePlanet model =
       let
           screen =
               model.screen
       in
       { model
           | planet =
               stepVel model.planet
                   |> gravitateTo model.sun
                   |> clampVelocity 30
                   |> bounceOffScreen screen
       }
-}


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
                { p | x = s.l, vx = p.vx * -1 * bulletWallDrag }

            else if p.x > s.r then
                { p | x = s.r, vx = p.vx * -1 * bulletWallDrag }

            else
                p

        bounceY p =
            if p.y < s.t then
                { p | y = s.t, vy = p.vy * -1 * bulletWallDrag }

            else if p.y > s.b then
                { p | y = s.b, vy = p.vy * -1 * bulletWallDrag }

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
        , renderSun model.sun
        , let
            factor =
                1 / model.fireRateInTicks * model.ticksSinceLastFire
          in
          renderTurret factor model.turret
        , g [] (List.map renderTurretBullet model.bullets)

        --, renderPlanet model.planet
        ]


renderSun { x, y, radius } =
    renderCircle x y radius [ fillColor Color.yellow ]


renderTurret : Float -> Turret -> TSC.Svg msg
renderTurret fireNextBulletProgress { x, y, radius, color } =
    let
        innerR =
            radius * fireNextBulletProgress
    in
    g []
        [ g [ transform [ Translate x y ] ]
            [ renderCircle 0 0 radius [ fillColor color ]
            , renderCircle 0 0 innerR [ fillColor <| whiteA 0.5 ]
            ]
        ]


renderTurretBullet { x, y, radius } =
    renderCircle x y radius [ fillColor <| whiteA 0.9 ]


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


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
