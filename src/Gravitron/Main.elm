module Gravitron.Main exposing (main)

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
import Task
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
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



-- GEOMETRY


type alias QPixels =
    Quantity Float Pixels.Pixels


type alias Direction =
    Direction2d ()


type alias Point =
    Point2d Pixels ()


pointAtXY : Float -> Float -> Point
pointAtXY x y =
    Point2d.xy (Pixels.pixels x) (Pixels.pixels y)



-- Radius


type alias Radius =
    Quantity Float Pixels


initRadius : Float -> Radius
initRadius =
    Pixels.pixels



-- HasRadius


type alias HasRadius a =
    { a | radius : Radius }


addRadii : HasRadius a -> HasRadius b -> Radius
addRadii c1 c2 =
    Quantity.plus c1.radius c2.radius



-- HasPosition


type alias HasPosition a =
    { a | position : Point }


mapPosition : (Point -> Point) -> HasPosition a -> HasPosition a
mapPosition func model =
    { model | position = func model.position }


distanceBetweenPositions : HasPosition a -> HasPosition b -> QPixels
distanceBetweenPositions c1 c2 =
    Point2d.distanceFrom c1.position c2.position



-- Velocity


type alias Velocity =
    Vector2d Pixels ()


velocityFromMagnitudeDirection : Radius -> Direction -> Velocity
velocityFromMagnitudeDirection =
    Vector2d.withLength


velocityRadius : Velocity -> Radius
velocityRadius =
    Vector2d.length


velocityMapMagnitude : (Radius -> Radius) -> Velocity -> Velocity
velocityMapMagnitude func model =
    let
        radius : Radius
        radius =
            velocityRadius model
                |> func

        direction =
            Vector2d.direction model
                |> Maybe.withDefault (Direction2d.degrees 0)
    in
    Vector2d.withLength radius direction



-- HasVelocity


type alias HasVelocity a =
    { a | velocity : Velocity }


mapVelocity : (Velocity -> Velocity) -> HasVelocity a -> HasVelocity a
mapVelocity func model =
    { model | velocity = func model.velocity }


clampVelocityMagnitude : Float -> HasVelocity a -> HasVelocity a
clampVelocityMagnitude n =
    let
        maxRadius =
            initRadius n

        clampRadiusFunc =
            Quantity.min maxRadius
    in
    mapVelocity (velocityMapMagnitude clampRadiusFunc)



-- HasPositionVelocity


type alias HasPositionVelocity a =
    HasPosition { a | velocity : Velocity }


translatePositionByVelocity : HasPositionVelocity a -> HasPositionVelocity a
translatePositionByVelocity =
    with (.velocity >> Point2d.translateBy) mapPosition


type alias HasPositionRadius a =
    { a
        | position : Point
        , radius : Radius
    }


areCirclesOverlapping : HasPositionRadius a -> HasPositionRadius b -> Bool
areCirclesOverlapping c1 c2 =
    distanceBetweenPositions c1 c2
        |> Quantity.lessThanOrEqualTo (addRadii c1 c2)



-- DATA


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
    { position = pointAtXY x y
    , radius = initRadius 20
    , color = Color.lightGreen
    }


type BulletState
    = BulletTraveling


type alias Bullet =
    { position : Point
    , velocity : Velocity
    , radius : Radius
    , state : BulletState
    }


initBullet : Point -> Float -> Direction -> Bullet
initBullet position speed direction =
    { position = position
    , velocity = velocityFromMagnitudeDirection (initRadius speed) direction
    , radius = initRadius 5
    , state = BulletTraveling
    }



-- Model


type alias Flags =
    ()


type alias Model =
    { turret : Turret
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
init _ =
    ( { sun = initSun
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
              onTick model
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


onTick : Model -> Model
onTick model =
    phase1TranslatePositions model
        |> phase2HandleCollisions
        |> phase3UpdatePositionDependenciesForNextTick


phase1TranslatePositions : Model -> Model
phase1TranslatePositions ({ sun, bullets } as model) =
    { model
        | sun = translatePositionByVelocity sun
        , bullets = List.map translatePositionByVelocity bullets
    }


phase2HandleCollisions : Model -> Model
phase2HandleCollisions ({ screen, mouse, sun, bullets } as model) =
    let
        newBullets =
            let
                updateBullet bullet =
                    if areCirclesOverlapping sun bullet then
                        Nothing

                    else
                        Just bullet
            in
            List.filterMap updateBullet bullets
                |> List.map (bounceOffScreen screen)
    in
    { model | bullets = newBullets }


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
            in
            initBullet bPos bulletInitialSpeed dir

        appendNewBulletIfFired =
            shouldFireBullet
                |> Maybe.map (newBullet >> (::))
                |> Maybe.withDefault identity

        newBullets =
            let
                updateBullet : Bullet -> Bullet
                updateBullet bullet =
                    bullet
                        |> gravitateTo sun
                        |> applyDrag bulletUpdateDrag
                        |> clampVelocityMagnitude bulletMaxSpeed
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


applyDrag drag =
    mapVelocity (Vector2d.scaleBy drag)


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

        toParts { position, velocity } =
            let
                { x, y } =
                    Point2d.toPixels position

                ( vx, vy ) =
                    Vector2d.toTuple Pixels.inPixels velocity
            in
            { x = x, y = y, vx = vx, vy = vy }

        fromParts { x, y, vx, vy } =
            { position = pointAtXY x y, velocity = Vector2d.pixels vx vy }

        mapPositionVelocityAsParts func model =
            let
                { position, velocity } =
                    toParts model
                        |> func
                        |> fromParts
            in
            { model | position = position, velocity = velocity }
    in
    mapPositionVelocityAsParts (bounceX >> bounceY)


gravitateTo p2 p1 =
    let
        gravityVector =
            gravityVectorTo p2 p1
    in
    accelerate gravityVector p1


accelerate v2 =
    mapVelocity (Vector2d.plus v2)


gravityVectorTo p2 p1 =
    let
        p2Pos =
            p2.position |> Point2d.toPixels

        p2x =
            p2Pos.x

        p2y =
            p2Pos.y

        p1Pos =
            p1.position |> Point2d.toPixels

        p1x =
            p1Pos.x

        p1y =
            p1Pos.y

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

        gDirection =
            angleToP2
    in
    velocityFromMagnitudeDirection (initRadius gRadius) (Direction2d.radians gDirection)



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


renderTurretBullet { position, radius } =
    Draw.circle2d [ fillColor <| whiteA 0.9 ] (Circle2d.atPoint position radius)


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


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
