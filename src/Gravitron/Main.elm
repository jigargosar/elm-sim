module Gravitron.Main exposing (main)

import Angle
import Browser
import Browser.Dom
import Browser.Events
import Circle2d
import Color
import Direction2d exposing (Direction2d)
import Geometry.Svg as Draw
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import PointFree exposing (with)
import Quantity exposing (Quantity)
import Random exposing (Seed)
import Task
import TypedSvg exposing (circle, g, rect, svg)
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core as TSC
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..))
import Vector2d exposing (Vector2d)



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


mapPosition : (a -> a) -> { b | position : a } -> { b | position : a }
mapPosition func model =
    { model | position = func model.position }


positionFromXY { x, y } =
    Point2d.fromPixels { x = x, y = y }


translatePositionByVelocity =
    with (.velocity >> Point2d.translateBy) mapPosition


type alias QPixels =
    Quantity Float Pixels.Pixels


type alias Point =
    Point2d Pixels ()


type alias Velocity =
    Vector2d Pixels ()


type alias Radius =
    Quantity Float Pixels


type alias Sun =
    { position : Point
    , velocity : Velocity
    , radius : Radius
    , mass : Float
    }


initSun : Sun
initSun =
    { position = Point2d.pixels 0 0
    , velocity = Vector2d.pixels 0 0
    , radius = Pixels.pixels 20
    , mass = initialSunMass
    }


type alias Turret =
    { position : Point
    , radius : Radius
    , color : Color.Color
    }


initTurretAtXY : Float -> Float -> Turret
initTurretAtXY x y =
    { position = pointFromXY x y
    , radius = initPx 20
    , color = Color.lightGreen
    }


initPx : number -> Quantity number Pixels
initPx =
    Pixels.pixels


type BulletState
    = BulletTraveling


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
      , turret = initTurretAtXY -100 -100
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
    phase1TranslatePositions model
        |> phase2UpdateCollisions
        |> phase3UpdatePositionDependenciesForNextTick


phase1TranslatePositions : Model -> Model
phase1TranslatePositions ({ sun, bullets } as model) =
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
                    if areCirclesOverlapping sun (bulletToPositionRadius bullet) then
                        Nothing

                    else
                        Just bullet
            in
            List.filterMap updateBullet bullets
                |> List.map (bounceOffScreen screen)
    in
    { model | bullets = newBullets }


areCirclesOverlapping : HasPositionRadius a -> HasPositionRadius b -> Bool
areCirclesOverlapping c1 c2 =
    distanceFrom c1 c2
        |> Quantity.lessThanOrEqualTo (addRadii c1 c2)


addRadii : HasRadius a -> HasRadius b -> Radius
addRadii c1 c2 =
    Quantity.plus c1.radius c2.radius


distanceFrom : HasPosition a -> HasPosition b -> QPixels
distanceFrom c1 c2 =
    Point2d.distanceFrom c1.position c2.position


type alias HasPosition a =
    { a | position : Point }


type alias HasRadius a =
    { a | radius : Radius }


type alias HasPositionRadius a =
    { a
        | position : Point
        , radius : Radius
    }


pointFromXY : Float -> Float -> Point
pointFromXY x y =
    Point2d.xy (Pixels.pixels x) (Pixels.pixels y)


sunUpdateVelocityTowards point model =
    { model
        | velocity =
            Vector2d.from model.position point
                |> Vector2d.scaleBy 0.1
    }


phase3UpdatePositionDependenciesForNextTick : Model -> Model
phase3UpdatePositionDependenciesForNextTick ({ screen, mouse, sun, turret, bullets } as model) =
    let
        mousePosition =
            Point2d.fromPixels mouse

        newSun =
            sunUpdateVelocityTowards mousePosition sun

        ( shouldFireBullet, newTicksSinceFire ) =
            updateTicksSinceLastFire model

        newBullet _ =
            let
                dir =
                    Direction2d.from turret.position sun.position
                        |> Maybe.withDefault Direction2d.x
                        |> always (Direction2d.degrees 190)

                bPos =
                    Point2d.translateIn
                        dir
                        turret.radius
                        turret.position
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
        p2Pos =
            p2.position |> Point2d.toPixels

        p2x =
            p2Pos.x

        p2y =
            p2Pos.y

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


renderSun { position, radius } =
    Draw.circle2d
        [ fillColor Color.yellow ]
        (Circle2d.atPoint position radius)


renderTurret : Float -> Turret -> TSC.Svg msg
renderTurret fireNextBulletProgress { position, radius, color } =
    let
        innerR =
            radius |> Quantity.multiplyBy fireNextBulletProgress
    in
    g []
        [ Draw.circle2d [ fillColor color ] (Circle2d.atPoint position radius)
        , Draw.circle2d [ fillColor <| whiteA 0.5 ] (Circle2d.atPoint position innerR)
        ]


type alias Circle =
    Circle2d.Circle2d Pixels ()


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
