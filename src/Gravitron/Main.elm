module Gravitron.Main exposing (main)

import Angle
import Browser
import Browser.Dom
import Browser.Events
import Circle2d
import Color
import Direction2d exposing (Direction2d)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import PointFree exposing (with)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Task
import TypedSvg exposing (g, svg)
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



-- Position


type Position
    = Position Float Float


positionXY : Float -> Float -> Position
positionXY =
    Position


positionDistanceSquaredFrom : Position -> Position -> Float
positionDistanceSquaredFrom (Position x1 y1) (Position x2 y2) =
    (x2 - x1) ^ 2 + (y2 - y1) ^ 2


positionDistanceFrom : Position -> Position -> Float
positionDistanceFrom p1 p2 =
    positionDistanceSquaredFrom p1 p2 |> sqrt


angleBetweenPositions : Position -> Position -> Float
angleBetweenPositions (Position x1 y1) (Position x2 y2) =
    atan2 (y2 - y1) (x2 - x1)


positionToTuple : Position -> ( Float, Float )
positionToTuple (Position x y) =
    ( x, y )


positionFromPoint : Point -> Position
positionFromPoint point =
    let
        { x, y } =
            Point2d.toPixels point
    in
    positionXY x y


positionToPoint : Position -> Point
positionToPoint (Position x y) =
    Point2d.pixels x y



-- Radius


type Radius
    = Radius Float


newRadius : Float -> Radius
newRadius =
    abs >> Radius


radiusAdd2 : Radius -> Radius -> Radius
radiusAdd2 (Radius r1) (Radius r2) =
    r1 + r2 |> newRadius


radiusToQPixels : Radius -> QPixels
radiusToQPixels (Radius r) =
    pixels r


radiusToFloat : Radius -> Float
radiusToFloat (Radius r) =
    r


radiusScaleBy : Float -> Radius -> Radius
radiusScaleBy scale (Radius r) =
    r * abs scale |> Radius



-- HasRadius


type alias HasRadius a =
    { a | radius : Radius }


addRadii : HasRadius a -> HasRadius b -> Radius
addRadii c1 c2 =
    radiusAdd2 c1.radius c2.radius



-- HasPosition


type alias HasPosition a =
    { a | position : Position }


mapPosition : (Position -> Position) -> HasPosition a -> HasPosition a
mapPosition func model =
    { model | position = func model.position }


mapPositionAsPoint : (Point -> Point) -> HasPosition a -> HasPosition a
mapPositionAsPoint func =
    mapPosition (positionToPoint >> func >> positionFromPoint)


distanceBetweenPositions : HasPosition a -> HasPosition b -> Float
distanceBetweenPositions c1 c2 =
    positionDistanceFrom c1.position c2.position



-- Velocity


type alias Velocity =
    Vector2d Pixels ()


velocityFromSpeedDirection : QPixels -> Direction -> Velocity
velocityFromSpeedDirection =
    Vector2d.withLength


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
    with (.velocity >> Point2d.translateBy) mapPositionAsPoint


type alias HasPositionRadius a =
    HasPosition (HasRadius a)


areCirclesOverlapping : HasPositionRadius a -> HasPositionRadius b -> Bool
areCirclesOverlapping c1 c2 =
    distanceBetweenPositions c1 c2 <= (addRadii c1 c2 |> radiusToFloat)



-- DATA


type alias Sun =
    HasPosition (HasVelocity (HasRadius { mass : Float }))


initSun : Sun
initSun =
    { position = positionXY 0 0
    , velocity = Vector2d.pixels 0 0
    , radius = newRadius 20
    , mass = initialSunMass
    }


type alias Turret =
    HasPosition (HasRadius { color : Color.Color })


initTurretAtXY : Float -> Float -> Turret
initTurretAtXY x y =
    { position = positionXY x y
    , radius = newRadius 20
    , color = Color.lightGreen
    }


type BulletState
    = BulletTraveling


type alias Bullet =
    HasPosition (HasVelocity (HasRadius { state : BulletState }))


initBullet : Position -> Float -> Direction -> Bullet
initBullet position speed direction =
    { position = position
    , velocity = velocityFromSpeedDirection (pixels speed) direction
    , radius = newRadius 5
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
            Vector2d.from (positionToPoint model.position) point
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
                    Direction2d.from (positionToPoint turret.position) (positionToPoint sun.position)
                        |> Maybe.withDefault Direction2d.x
                        |> always (Direction2d.degrees 190)

                bPos =
                    Point2d.translateIn
                        dir
                        (turret.radius |> radiusToQPixels)
                        (positionToPoint turret.position)
            in
            initBullet (positionFromPoint bPos) bulletInitialSpeed dir

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


bounceOffScreen : Screen -> HasPositionVelocity a -> HasPositionVelocity a
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
                ( x, y ) =
                    positionToTuple position

                ( vx, vy ) =
                    Vector2d.toTuple Pixels.inPixels velocity
            in
            { x = x, y = y, vx = vx, vy = vy }

        fromParts { x, y, vx, vy } =
            { position = positionXY x y, velocity = Vector2d.pixels vx vy }

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


gravitateTo : Sun -> HasPositionVelocity a -> HasPositionVelocity a
gravitateTo p2 p1 =
    let
        gravityVector =
            gravityVectorTo p2 p1
    in
    mapVelocity (Vector2d.plus gravityVector) p1


gravityVectorTo : Sun -> HasPosition a -> Vector2d Pixels ()
gravityVectorTo p2 p1 =
    let
        angle =
            angleBetweenPositions p1.position p2.position

        magnitude =
            p2.mass
                / positionDistanceSquaredFrom p1.position p2.position
    in
    Vector2d.rTheta (pixels magnitude) (Angle.radians angle)



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


canvas : Screen -> List (Svg Msg) -> Svg Msg
canvas { l, t, w, h } =
    svg [ style "position" "fixed", viewBox l t w h, width w, height h ]


renderBullets : List Bullet -> Svg Msg
renderBullets bullets =
    g [] (List.map renderBullet bullets)


renderBackground : Screen -> Svg Msg
renderBackground { l, t, w, h } =
    Svg.rect [ x l, y t, width w, height h, fillColor Color.black ] []


renderSun : Sun -> Svg Msg
renderSun { position, radius } =
    drawCircle [ fillColor Color.yellow ] position radius


renderTurret : Float -> Turret -> Svg msg
renderTurret fireNextBulletProgress { position, radius, color } =
    let
        innerR =
            radiusScaleBy fireNextBulletProgress radius
    in
    g []
        [ drawCircle [ fillColor color ] position radius
        , drawCircle [ fillColor <| whiteA 0.5 ] position innerR
        ]


renderBullet : Bullet -> Svg Msg
renderBullet { position, radius } =
    drawCircle [ fillColor <| whiteA 0.9 ] position radius



-- Drawing Helpers


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


fillColor : Color.Color -> Svg.Attribute msg
fillColor =
    Fill >> fill


drawCircle : List (Svg.Attribute msg) -> Position -> Radius -> Svg msg
drawCircle attrs position radius =
    Geometry.Svg.circle2d attrs (Circle2d.atPoint (positionToPoint position) (radiusToQPixels radius))



-- Program


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
