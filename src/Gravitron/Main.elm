module Gravitron.Main exposing (main)

import Angle
import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom
import Browser.Events
import Circle2d
import Color
import Direction2d exposing (Direction2d)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD
import LineSegment2d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Random exposing (Seed)
import Task
import TypedSvg exposing (circle, g, rect, svg)
import TypedSvg.Attributes exposing (fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core as TSC
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..))
import Vector2d



-- Constants


turretFireRateInTicks =
    10


bulletInitialSpeed =
    5


bulletMaxSpeed =
    100000


initialSunMass =
    1500


bulletWallDrag =
    1


bulletUpdateDrag =
    1



-- DATA


type alias Point =
    Point2d Pixels ()


type alias Sun =
    { position : Point
    , vx : Float
    , vy : Float
    , radius : Float
    , mass : Float
    }


mapPosition : (a -> a) -> { b | position : a } -> { b | position : a }
mapPosition func model =
    { model | position = func model.position }


velocityVectorFromVXY { vx, vy } =
    Vector2d.fromPixels { x = vx, y = vy }


positionFromXY { x, y } =
    Point2d.fromPixels { x = x, y = y }


translatePositionByVelocity =
    with (velocityVectorFromVXY >> Point2d.translateBy) mapPosition


with func1 func2 model =
    func2 (func1 model) model


initSun : Sun
initSun =
    { position = Point2d.pixels 0 0
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


type BulletState
    = BulletTraveling
    | BulletExploding


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    , state : BulletState
    }


bulletToPositionRadius b =
    { position = positionFromXY b, radius = Pixels.pixels b.radius }


sunToPositionRadius s =
    { position = s.position, radius = Pixels.pixels s.radius }


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
    , state = BulletTraveling
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
      , turret = initTurretAt -100 -100
      , ticksSinceLastFire = 0
      , fireRateInTicks = turretFireRateInTicks
      , bullets = []
      , mouse = Mouse 100 100
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
            ( -- updateOnTick model
              phasedUpdateOnTick model
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


phasedUpdateOnTick : Model -> Model
phasedUpdateOnTick model =
    phase1UpdatePositions model
        |> phase2UpdateCollisions
        |> phase3UpdatePositionDependenciesForNextTick


phase1UpdatePositions : Model -> Model
phase1UpdatePositions ({ sun, bullets } as model) =
    { model
        | sun = translatePositionByVelocity sun
        , bullets = List.map stepVel bullets
    }


phase2UpdateCollisions : Model -> Model
phase2UpdateCollisions ({ screen, mouse, sun, bullets } as model) =
    let
        newBullets =
            let
                updateBullet bullet =
                    if areCirclesOverlapping (sunToPositionRadius sun) (bulletToPositionRadius bullet) then
                        Nothing

                    else
                        Just bullet
            in
            List.filterMap updateBullet bullets
                |> List.map (bounceOffScreen screen)
    in
    { model | bullets = newBullets }


areCirclesOverlapping c1 c2 =
    Vector2d.from c1.position c2.position
        |> Vector2d.length
        |> Quantity.lessThanOrEqualTo (mapEach .radius ( c1, c2 ) |> uncurry Quantity.plus)


mapEach func ( a, b ) =
    ( func a, func b )


distanceSquared : { a | x : number, y : number } -> { b | x : number, y : number } -> number
distanceSquared p1 p2 =
    (p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2



{-
   distance : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Float
   distance p1 p2 =
       distanceSquared p1 p2 |> sqrt
-}


pt : { a | x : Float, y : Float } -> Point2d.Point2d Pixels coordinates
pt { x, y } =
    Point2d.fromPixels { x = x, y = y }


phase3UpdatePositionDependenciesForNextTick : Model -> Model
phase3UpdatePositionDependenciesForNextTick ({ screen, mouse, sun, turret, bullets } as model) =
    let
        newSun =
            followXY mouse sun

        ( shouldFireBullet, newTicksSinceFire ) =
            updateTicksSinceLastFire model

        newBullet _ =
            let
                { x, y } =
                    turret

                dir =
                    Direction2d.from (pt turret) (pt sun)
                        |> Maybe.withDefault Direction2d.x
                        |> always (Direction2d.degrees 190)

                bPos =
                    Point2d.translateIn
                        dir
                        (Pixels.pixels turret.radius)
                        (pt turret)
                        |> Point2d.toPixels

                bAngle =
                    Direction2d.toAngle dir
                        |> Angle.inRadians
            in
            initBullet bPos.x bPos.y bulletInitialSpeed bAngle

        appendNewBulletIfFired =
            shouldFireBullet
                |> Maybe.map (newBullet >> (::))
                |> Maybe.withDefault identity

        newBullets =
            let
                updateBullet bullet =
                    bullet
                        |> gravitateTo sun
                        |> applyDrag bulletUpdateDrag
                        |> clampVelocity bulletMaxSpeed
            in
            bullets
                |> List.map updateBullet
                |> appendNewBulletIfFired
    in
    { model
        | sun = newSun
        , ticksSinceLastFire = newTicksSinceFire
        , bullets = newBullets
    }



--updateOnTick : Model -> Model
--updateOnTick ({ screen, mouse, sun, bullets } as model) =
--    let
--        newSun =
--            sun
--                |> stepVel
--                |> followXY mouse
--
--        ( shouldFireBullet, newTicksSinceFire ) =
--            updateTicksSinceLastFire model
--
--        appendNewBulletIfFired =
--            shouldFireBullet
--                |> Maybe.map (\_ -> (::) (initBullet 0 0 bulletInitialSpeed (degrees 180)))
--                |> Maybe.withDefault identity
--
--        newBullets =
--            let
--                updateBullet bullet =
--                    bullet
--                        |> stepVel
--                        |> gravitateTo sun
--                        |> applyDrag bulletUpdateDrag
--                        |> clampVelocity bulletMaxSpeed
--                        |> bounceOffScreen screen
--            in
--            bullets
--                |> appendNewBulletIfFired
--                |> List.map updateBullet
--    in
--    { model
--        | sun = newSun
--        , ticksSinceLastFire = newTicksSinceFire
--        , bullets = newBullets
--    }


type FireBullet
    = FireBullet


updateTicksSinceLastFire :
    { a | ticksSinceLastFire : Float, fireRateInTicks : Float }
    -> ( Maybe FireBullet, Float )
updateTicksSinceLastFire { ticksSinceLastFire, fireRateInTicks } =
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
