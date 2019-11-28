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
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes as TSA exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core as TSC
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..))



-- Constants


turretAimSpeed =
    degrees 2


bulletInitialFireRate =
    60


bulletInitialSpeed =
    1


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
    { x : Float
    , y : Float
    , radius : Float
    , color : Color.Color
    , elapsed : Float
    , rate : Float
    , bullets : List Bullet
    , aimAngle : Float
    , aimTargetAngle : Float
    , seed : Seed
    }


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    }


initTurret : Seed -> Turret
initTurret seed =
    { x = -100
    , y = 100
    , radius = 20
    , color = Color.lightGreen
    , elapsed = 0
    , rate = bulletInitialFireRate
    , bullets = []
    , aimAngle = 0
    , aimTargetAngle = 0
    , seed = seed
    }


turretGenerator : Random.Generator Turret
turretGenerator =
    Random.independentSeed
        |> Random.map
            (initTurret
                >> turretAimTowardsRandomAngle
                >> (\turret -> { turret | aimAngle = turret.aimTargetAngle })
            )


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

    --, planet : Planet
    , turret : Turret
    , sun : Sun
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
    let
        ( turret, seed ) =
            Random.step turretGenerator (Random.initialSeed flags.now)
    in
    ( { seed = seed
      , sun = initSun

      --, planet = initPlanet
      , turret = turret
      , ct = 0
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
            ( { model | ct = model.ct + 1 }
                --|> updatePlanet
                |> updateTurret
                |> updateSun
                |> updateCollisions
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


updateCollisions : Model -> Model
updateCollisions model =
    let
        areColliding c1 c2 =
            let
                dx =
                    c2.x - c1.x

                dy =
                    c2.y - c1.y

                distanceSquared =
                    dx ^ 2 + dy ^ 2

                radiiSquared =
                    (c1.radius + c2.radius) ^ 2
            in
            distanceSquared <= radiiSquared

        folder bullet ( sun, bullets ) =
            if areColliding sun bullet then
                ( sun, bullets )

            else
                ( sun, bullet :: bullets )

        turret =
            model.turret
    in
    List.foldl folder ( model.sun, [] ) turret.bullets
        |> (\( sun, bullets ) ->
                { model
                    | turret = { turret | bullets = bullets }
                    , sun = sun
                }
           )


type alias HasSeed a =
    { a | seed : Seed }


stepRandom : Random.Generator a -> HasSeed b -> ( a, HasSeed b )
stepRandom generator model =
    Random.step generator model.seed
        |> Tuple.mapSecond (\seed -> { model | seed = seed })


randomAngle =
    Random.float 0 (degrees 359)


updateTurret : Model -> Model
updateTurret model =
    { model
        | turret =
            model.turret
                |> updateTurretElapsed
                |> updateTurretAim
                |> turretWhenBulletReadyUpdateWith
                    (turretFireBullet >> turretAimTowardsRandomAngle)
                |> updateTurretBullets model
    }


fModBy fixedPt roller =
    roller + (toFloat <| ceiling (-roller / fixedPt)) * fixedPt


updateTurretAim : Turret -> Turret
updateTurretAim turret =
    let
        s =
            turret.aimAngle

        e =
            turret.aimTargetAngle

        _ =
            if s < 0 || s > degrees 360 || e < 0 || e > degrees 360 then
                Debug.log "end > 360" ( s, e )
                    |> Debug.todo "wtf"

            else
                1
    in
    {- if s == e then
           turret

       else
    -}
    let
        speed =
            turretAimSpeed

        final =
            if s < e then
                let
                    da =
                        e - s
                in
                {- if da <= speed then
                       e

                   else
                -}
                if da < pi then
                    s + speed

                else
                    s - speed

            else
                let
                    da =
                        s - e
                in
                {- if da <= speed then
                       e

                   else
                -}
                if da < pi then
                    s - speed

                else
                    s + speed
    in
    { turret | aimAngle = final |> fModBy (degrees 360) }


updateTurretElapsed : Turret -> Turret
updateTurretElapsed turret =
    { turret | elapsed = turret.elapsed + 1 }


turretWhenBulletReadyUpdateWith func turret =
    if turret.elapsed >= turret.rate then
        turret
            |> turretResetElapsed
            |> func

    else
        turret


turretAimTowardsRandomAngle : Turret -> Turret
turretAimTowardsRandomAngle =
    stepRandom randomAngle >> apply2 turretAimToWards


turretAimToWards : Float -> Turret -> Turret
turretAimToWards angle turret =
    { turret | aimTargetAngle = angle }


turretResetElapsed : Turret -> Turret
turretResetElapsed turret =
    { turret | elapsed = 0 }


turretFireBullet : Turret -> Turret
turretFireBullet turret =
    let
        x =
            turret.x

        y =
            turret.y

        angle =
            turret.aimAngle
    in
    { turret
        | bullets = initBullet x y bulletInitialSpeed angle :: turret.bullets
    }


updateTurretBullets : Model -> Turret -> Turret
updateTurretBullets model turret =
    let
        screen =
            model.screen

        sun =
            model.sun

        updateBullet bullet =
            bullet
                |> stepVel
                |> gravitateTo sun
                |> applyDrag bulletUpdateDrag
                |> clampVelocity bulletMaxSpeed
                |> bounceOffScreen screen
    in
    { turret | bullets = List.map updateBullet turret.bullets }


applyDrag drag p =
    { p | vx = p.vx * drag, vy = p.vy * drag }


updateSun model =
    { model
        | sun =
            model.sun
                |> stepVel
                |> followXY model.mouse
    }


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


angleTo : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Float
angleTo p2 p1 =
    let
        dxy =
            ( p2.x - p1.x, p1.y - p2.y )
    in
    dxy
        |> apply2 atan2
        |> (+) (pi / 2)


apply2 : (a -> b -> c) -> ( a, b ) -> c
apply2 func ( a, b ) =
    func a b


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
        , renderTurret model.turret

        --, renderPlanet model.planet
        ]


renderSun { x, y, radius } =
    renderCircle x y radius [ fillColor Color.yellow ]


renderTurret : Turret -> TSC.Svg msg
renderTurret { x, y, radius, color, rate, aimAngle, aimTargetAngle, elapsed, bullets } =
    let
        innerR =
            radius / rate * elapsed
    in
    g []
        [ g [ transform [ Translate x y ] ]
            [ renderCircle 0 0 radius [ fillColor color ]
            , renderCircle 0 0 innerR [ fillColor <| whiteA 0.5 ]
            , let
                ( p2x, p2y ) =
                    fromPolar ( radius, aimAngle )
              in
              line [ x1 0, y1 0, x2 p2x, y2 p2y, stroke Color.red, strokeWidth 4 ] []
            , let
                ( p2x, p2y ) =
                    fromPolar ( radius, aimTargetAngle )
              in
              line [ x1 0, y1 0, x2 p2x, y2 p2y, stroke Color.orange, strokeWidth 3 ] []
            ]
        , g [] (List.map renderTurretBullet bullets)
        ]


renderTurretBullet { x, y, radius } =
    renderCircle x y radius [ fillColor <| whiteA 0.9 ]


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


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
