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
import Svg exposing (Svg)
import Task
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
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



-- ELM GEOMETRY


type alias QPixels =
    Quantity Float Pixels.Pixels


pixels : Float -> QPixels
pixels =
    Pixels.pixels


type alias Direction =
    Direction2d ()


type alias Point =
    Point2d Pixels ()


pointXY : Float -> Float -> Point
pointXY x y =
    Point2d.xy (pixels x) (pixels y)



-- Position


type Position
    = Position Float Float


positionXY : Float -> Float -> Position
positionXY =
    Position


positionToPoint : Position -> Point
positionToPoint (Position x y) =
    pointXY x y



-- Radius


type Radius
    = Radius Float


initRadius : Float -> Radius
initRadius =
    abs >> Radius


radiusAdd2 : Radius -> Radius -> Radius
radiusAdd2 (Radius r1) (Radius r2) =
    r1 + r2 |> initRadius


radiusToQPixels : Radius -> QPixels
radiusToQPixels (Radius r) =
    pixels r



-- HasRadius


type alias HasRadius a =
    { a | radius : Radius }


addRadii : HasRadius a -> HasRadius b -> Radius
addRadii c1 c2 =
    radiusAdd2 c1.radius c2.radius



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
velocityFromMagnitudeDirection r =
    Vector2d.withLength (radiusToQPixels r)


velocityMagnitude : Velocity -> QPixels
velocityMagnitude =
    Vector2d.length


velocityMapMagnitude : (QPixels -> QPixels) -> Velocity -> Velocity
velocityMapMagnitude func model =
    let
        magnitude : QPixels
        magnitude =
            velocityMagnitude model |> func

        direction =
            Vector2d.direction model
                |> Maybe.withDefault (Direction2d.degrees 0)
    in
    Vector2d.withLength magnitude direction



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
            pixels n

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
        |> Quantity.lessThanOrEqualTo (addRadii c1 c2 |> radiusToQPixels)



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
    , radius = initRadius 20
    , mass = initialSunMass
    }


type alias Turret =
    { position : Point
    , radius : Radius
    , color : Color.Color
    }


initTurretAtXY : Float -> Float -> Turret
initTurretAtXY x y =
    { position = pointXY x y
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
                        (turret.radius |> radiusToQPixels)
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
            { position = pointXY x y, velocity = Vector2d.pixels vx vy }

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
        vectorToP2 =
            Vector2d.from p1.position p2.position

        directionToP2 =
            Vector2d.direction vectorToP2
                |> Maybe.withDefault Direction2d.x

        distanceSquareToP2InPixels =
            Vector2d.length vectorToP2
                --|> Quantity.squared
                |> Pixels.inPixels
                |> (^) 2

        gMagnitude =
            p2.mass
                / distanceSquareToP2InPixels
                |> Pixels.pixels

        gDirection =
            directionToP2
    in
    Vector2d.withLength gMagnitude gDirection



-- View


view : Model -> Html Msg
view model =
    let
        screen =
            model.screen
    in
    canvas screen
        [ renderBackground screen
        , renderSun model.sun
        , let
            factor =
                1 / model.fireRateInTicks * model.ticksSinceLastFire
          in
          renderTurret factor model.turret
        , renderBullets model.bullets
        ]


renderBullets : List Bullet -> Svg Msg
renderBullets bullets =
    g [] (List.map renderBullet bullets)


canvas : Screen -> List (Svg Msg) -> Svg Msg
canvas { l, t, w, h } =
    svg [ style "position" "fixed", viewBox l t w h, width w, height h ]


renderBackground : Screen -> Svg Msg
renderBackground { l, t, w, h } =
    Svg.rect [ x l, y t, width w, height h, fillColor Color.black ] []


renderSun : Sun -> Svg Msg
renderSun { position, radius } =
    drawCircle [ fillColor Color.yellow ] position radius


renderTurret : Float -> Turret -> Svg msg
renderTurret fireNextBulletProgress { position, radius, color } =
    let
        radiusQPixels =
            radiusToQPixels radius

        innerR =
            radiusQPixels |> Quantity.multiplyBy fireNextBulletProgress
    in
    g []
        [ Draw.circle2d [ fillColor color ] (Circle2d.atPoint position radiusQPixels)
        , Draw.circle2d [ fillColor <| whiteA 0.5 ] (Circle2d.atPoint position innerR)
        ]


renderBullet : Bullet -> Svg Msg
renderBullet { position, radius } =
    Draw.circle2d [ fillColor <| whiteA 0.9 ] (Circle2d.atPoint position (radiusToQPixels radius))



-- Drawing Helpers


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


fillColor : Color.Color -> Svg.Attribute msg
fillColor =
    Fill >> fill


drawCircle : List (Svg.Attribute msg) -> Point2d Pixels coordinates -> Radius -> Svg msg
drawCircle attrs position radius =
    Draw.circle2d attrs (Circle2d.atPoint position (radiusToQPixels radius))



-- Program


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
