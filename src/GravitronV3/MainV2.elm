module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import PointFree exposing (rejectWhen)
import Random exposing (Generator, Seed)
import Random.Float
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Point


type Point
    = Point Vec


movePt : Vec -> Point -> Point
movePt vec (Point pt) =
    Point (Vec.add pt vec)


vecFromTo : Point -> Point -> Vec
vecFromTo (Point from) (Point to) =
    Vec.fromTo from to


pointAt : ( Float, Float ) -> Point
pointAt ( x, y ) =
    Point (Vec.vec x y)


pointToTuple : Point -> ( Float, Float )
pointToTuple (Point vec) =
    Vec.toTuple vec



-- mappers


setVelocity : a -> { b | velocity : a } -> { b | velocity : a }
setVelocity velocity model =
    { model | velocity = velocity }


mapVelocity : (b -> b) -> { a | velocity : b } -> { a | velocity : b }
mapVelocity func model =
    { model | velocity = func model.velocity }


type alias PosVel a =
    { a
        | position : Point
        , velocity : Vec
    }


mapVelocityWithPosition : (PosVel a -> Vec) -> PosVel a -> PosVel a
mapVelocityWithPosition func model =
    { model | velocity = func model }


mapPositionWithVelocity : (PosVel a -> Point) -> PosVel a -> PosVel a
mapPositionWithVelocity func model =
    { model | position = func model }


mapVelocityAndSeed : (b -> a -> ( b, a )) -> { c | velocity : b, seed : a } -> { c | velocity : b, seed : a }
mapVelocityAndSeed func model =
    let
        ( velocity, seed ) =
            func model.velocity model.seed
    in
    { model | velocity = velocity, seed = seed }



-- Helpers


circleCircleCollision : { a | position : Point, radius : Float } -> { b | position : Point, radius : Float } -> Bool
circleCircleCollision c1 c2 =
    Vec.lenFrom (c1.position |> pointToTuple |> Vec.fromTuple)
        (c2.position |> pointToTuple |> Vec.fromTuple)
        < (c1.radius + c2.radius)


translatePosByVel : PosVel a -> Point
translatePosByVel model =
    movePt model.velocity model.position


randomWalkerVelocity : Vec -> Generator Vec
randomWalkerVelocity velocity =
    let
        randomAngle : Generator Float
        randomAngle =
            Random.Float.standardNormal |> Random.map ((*) 0.005 >> turns)
    in
    randomAngle
        |> Random.map
            (\newAngleDiff ->
                velocity
                    |> Vec.mapAngle ((+) newAngleDiff)
                    |> Vec.mapMagnitude (max 0.01)
            )


stepRandomWalkerVelocity : Vec -> Seed -> ( Vec, Seed )
stepRandomWalkerVelocity velocity seed =
    Random.step (randomWalkerVelocity velocity) seed


bounceWithinScreenHelp : Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreenHelp screen position bounceFactor velocity =
    let
        bounceVelocityPart lo high positionPart velocityPart =
            if
                (positionPart < lo && velocityPart < 0)
                    || (positionPart > high && velocityPart > 0)
            then
                negate velocityPart

            else
                velocityPart

        ( x, y ) =
            Vec.toTuple position

        ( vx, vy ) =
            Vec.toTuple velocity

        newBouncedVelocity =
            vec (bounceVelocityPart screen.left screen.right x vx)
                (bounceVelocityPart screen.top screen.bottom y vy)
    in
    if velocity /= newBouncedVelocity then
        newBouncedVelocity |> Vec.mapMagnitude ((*) bounceFactor)

    else
        newBouncedVelocity


bounceWithinScreen : Env -> Float -> PosVel a -> Vec
bounceWithinScreen env factor m =
    bounceWithinScreenHelp env.screen (m.position |> (pointToTuple >> Vec.fromTuple)) factor m.velocity


gravitateTo : PosVel target -> PosVel model -> Vec
gravitateTo target model =
    model.velocity
        |> Vec.add
            (vecFromTo model.position target.position
                |> Vec.mapMagnitude (\mag -> 20 / mag)
            )



-- Player


type alias Player =
    { position : Point
    , velocity : Vec
    , radius : Float
    , seed : Seed
    }


initialPlayer : Player
initialPlayer =
    { position = pointAt ( 0, 0 )
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , seed = Random.initialSeed 1234
    }


updatePlayer : Env -> Player -> Player
updatePlayer env =
    mapVelocityAndSeed stepRandomWalkerVelocity
        >> mapVelocityWithPosition (bounceWithinScreen env 1)
        >> mapPositionWithVelocity translatePosByVel


viewPlayer : Player -> Shape
viewPlayer { position, radius } =
    viewFilledCircle "green" radius position



-- Bullet


type alias Bullet =
    { position : Point
    , velocity : Vec
    , radius : Float
    }


initBullet : Bullet
initBullet =
    { position = pointAt ( 0, 0 )
    , velocity = Vec.fromRTheta 3 0
    , radius = 10
    }


setPosVelFromTo :
    { a | position : Point, radius : Float }
    -> { b | position : Point }
    -> { c | radius : Float, position : Point, velocity : Vec }
    -> { c | radius : Float, position : Point, velocity : Vec }
setPosVelFromTo src target m =
    let
        angle =
            vecFromTo src.position target.position
                |> Vec.angle
    in
    { m
        | position =
            movePt (Vec.fromRTheta (m.radius + src.radius) angle)
                src.position
        , velocity = Vec.fromRTheta 3 angle
    }


isBulletIntersecting : BulletCtx bc -> Bullet -> Bool
isBulletIntersecting ctx b =
    circleCircleCollision b ctx.player



-- || List.any (circleCircleCollision b) [ ctx.turret ]


type alias BulletCtx a =
    { a
        | player : Player
        , turret : Turret
    }


updateBullets : Env -> BulletCtx bc -> List Bullet -> List Bullet
updateBullets env ctx =
    List.map
        (mapVelocityWithPosition (gravitateTo ctx.player)
            >> mapVelocityWithPosition (bounceWithinScreen env 0.5)
            >> mapPositionWithVelocity translatePosByVel
        )
        >> rejectWhen (isBulletIntersecting ctx)


viewBullet : Bullet -> Shape
viewBullet { position, radius } =
    viewFilledCircle "black" radius position
        |> fade 0.7



-- Turret


type alias Turret =
    { position : Point
    , velocity : Vec
    , radius : Float
    , bulletTimer : Timer
    }


initialTurret : Turret
initialTurret =
    { position = pointAt ( -150, -150 )
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Timer.start 0 60
    }


updateTurret : Env -> Player -> Turret -> ( List Bullet, Turret )
updateTurret env p turret =
    if Timer.isDone env.clock turret.bulletTimer then
        ( [ initBullet |> setPosVelFromTo turret p
          ]
        , { turret | bulletTimer = Timer.restart env.clock turret.bulletTimer }
        )

    else
        ( [], turret )


viewTurret : Turret -> Shape
viewTurret { position, radius } =
    viewFilledCircle "red" radius position



-- ViewHelpers


viewFilledCircle color radius position =
    circle radius
        |> fill color
        |> move (pointToTuple position)



-- Game


type alias Game =
    { player : Player
    , turret : Turret
    , bullets : List Bullet
    }


initialGame : Game
initialGame =
    { player = initialPlayer
    , turret = initialTurret
    , bullets = []
    }


updateGame : Env -> Game -> Game
updateGame env game =
    let
        ( genBullets, turret ) =
            updateTurret env game.player game.turret

        bullets =
            updateBullets env game game.bullets
    in
    { game
        | player = updatePlayer env game.player
        , turret = turret
        , bullets = genBullets ++ bullets
    }


viewGame : Game -> Shape
viewGame game =
    group
        [ viewPlayer game.player
        , viewTurret game.turret
        , List.map viewBullet game.bullets
            |> group
        ]



--


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view { screen, clock, game } =
    renderShapes screen
        [ viewGame game ]


type alias Model =
    { screen : Screen
    , mousePosition : Vec
    , clock : Float
    , game : Game
    }


type alias Env =
    { mousePosition : Vec
    , screen : Screen
    , clock : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen.initial
      , mousePosition = Vec.zero
      , clock = 0
      , game = initialGame
      }
    , Task.perform GotScreen Screen.get
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick _ ->
            let
                env : Env
                env =
                    { mousePosition = model.mousePosition
                    , screen = model.screen
                    , clock = model.clock
                    }
            in
            save
                { model
                    | game = updateGame env model.game
                    , clock = model.clock + 1
                }

        MouseMove pageX pageY ->
            let
                screen =
                    model.screen
            in
            save
                { model
                    | mousePosition = vec (pageX + screen.left) (pageY + screen.top)
                }


subscriptions _ =
    Sub.batch
        [ Screen.onResize GotScreen
        , E.onAnimationFrame Tick
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        ]



--noinspection ElmUnusedSymbol


timeTravelElement =
    TimeTravel.element Debug.toString Debug.toString TimeTravel.defaultConfig


main =
    --timeTravelElement
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
