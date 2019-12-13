module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody exposing (CircularBody, RigidBody)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import PointFree exposing (rejectWhen)
import Random exposing (Generator, Seed)
import Random.Float
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Helpers


randomWalkerVelocityGenHelp : Vec -> Generator Vec
randomWalkerVelocityGenHelp velocity =
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


randomWalk : RigidBody a -> Generator Vec
randomWalk { velocity } =
    randomWalkerVelocityGenHelp velocity


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


bounceWithinScreen : Env -> Float -> RigidBody a -> Vec
bounceWithinScreen env factor m =
    bounceWithinScreenHelp env.screen
        (m.position |> (Pt.toTuple >> Vec.fromTuple))
        factor
        m.velocity


gravitateTo : RigidBody target -> RigidBody model -> Vec
gravitateTo target model =
    model.velocity
        |> Vec.add
            (Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (\mag -> 20 / mag)
            )



-- Player


type alias Player =
    CircularBody
        { seed : Seed
        }


initialPlayer : Player
initialPlayer =
    { position = Pt.origin
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , seed = Random.initialSeed 1234
    }


updatePlayer : Env -> Player -> Player
updatePlayer env =
    RigidBody.stepWithSeed
        [ randomWalk
        , bounceWithinScreen env 1 >> Random.constant
        ]


viewPlayer : Player -> Shape
viewPlayer { position, radius } =
    viewFilledCircle "green" radius position



-- Bullet


type alias Bullet =
    CircularBody {}


initBullet : Bullet
initBullet =
    { position = Pt.origin
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
            Pt.vecFromTo src.position target.position
                |> Vec.angle
    in
    { m
        | position =
            Pt.moveBy (Vec.fromRTheta (m.radius + src.radius) angle)
                src.position
        , velocity = Vec.fromRTheta 3 angle
    }


isBulletIntersecting : BulletCtx bc -> List Bullet -> Bullet -> Bool
isBulletIntersecting ctx otherBullets bullet =
    RigidBody.doCircleOverlap bullet ctx.player
        || List.any (RigidBody.doCircleOverlap bullet) ctx.turrets
        || List.any (RigidBody.doCircleOverlap bullet) otherBullets


type alias BulletCtx a =
    { a
        | player : Player
        , turrets : List Turret
    }


updateBullet : Env -> BulletCtx bc -> Bullet -> Bullet
updateBullet env ctx =
    RigidBody.step
        [ gravitateTo ctx.player
        , bounceWithinScreen env 0.5
        ]


bulletToExplosion : Env -> Bullet -> Explosion
bulletToExplosion env bullet =
    newExplosion env.clock bullet.position (viewBulletShape bullet)


type alias BulletResponse =
    { explosions : List Explosion
    }


updateBullets : Env -> BulletCtx bc -> List Bullet -> ( BulletResponse, List Bullet )
updateBullets env ctx =
    let
        addExplosion bullet =
            Tuple.mapFirst
                (\res ->
                    { res
                        | explosions =
                            bulletToExplosion env bullet
                                :: res.explosions
                    }
                )

        reducer :
            ( Bullet, List Bullet )
            -> ( BulletResponse, List Bullet )
            -> ( BulletResponse, List Bullet )
        reducer ( bullet, otherBullets ) =
            if isBulletIntersecting ctx otherBullets bullet then
                addExplosion bullet

            else
                Tuple.mapSecond
                    ((::) (updateBullet env ctx bullet))
    in
    List.Extra.select
        >> List.foldr reducer ( BulletResponse [], [] )


viewBullet : Bullet -> Shape
viewBullet bullet =
    viewBulletShape bullet
        |> move (Pt.toTuple bullet.position)


viewBulletShape : Bullet -> Shape
viewBulletShape { radius } =
    group
        [ circle radius
            |> fill "black"
            |> fade 0.7
        ]



-- Turret


type alias Turret =
    { position : Point
    , velocity : Vec
    , radius : Float
    , bulletTimer : Timer
    }


initTurret : Float -> Point -> Turret
initTurret clock point =
    { position = point
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Timer.start clock 60
    }


type alias TurretResponse =
    { bullets : List Bullet }


type alias TurretCtx tc =
    { tc
        | player : Player
        , bullets : List Bullet
    }


updateTurret : Env -> TurretCtx tc -> Turret -> ( TurretResponse, Turret )
updateTurret env ctx turret =
    if Timer.isDone env.clock turret.bulletTimer then
        ( TurretResponse
            [ initBullet |> setPosVelFromTo turret ctx.player
            ]
        , { turret | bulletTimer = Timer.restart env.clock turret.bulletTimer }
        )

    else
        ( TurretResponse [], turret )


accumTurretResponseInto :
    TurretResponse
    -> ( TurretResponse, Turret )
    -> ( TurretResponse, Turret )
accumTurretResponseInto resAcc =
    Tuple.mapFirst
        (\{ bullets } -> { resAcc | bullets = resAcc.bullets ++ bullets })


isTurretIntersecting : TurretCtx tc -> Turret -> Bool
isTurretIntersecting ctx turret =
    List.any (RigidBody.doCircleOverlap turret) ctx.bullets



-- || RigidBody.doCircleOverlap turret ctx.player


updateTurrets : Env -> TurretCtx tc -> List Turret -> ( TurretResponse, List Turret )
updateTurrets env ctx =
    List.Extra.mapAccuml
        (\resAcc ->
            updateTurret env ctx
                >> accumTurretResponseInto resAcc
        )
        (TurretResponse [])
        >> Tuple.mapSecond (rejectWhen (isTurretIntersecting ctx))


viewTurret : Turret -> Shape
viewTurret { position, radius } =
    viewFilledCircle "red" radius position



-- Explosion


type alias Explosion =
    { position : Point
    , shape : Shape
    , timer : Timer
    }


newExplosion : Float -> Point -> Shape -> Explosion
newExplosion clock position shape =
    { position = position
    , shape = shape
    , timer = Timer.start clock 120
    }


updateExplosions : Env -> List Explosion -> List Explosion
updateExplosions env =
    rejectWhen (.timer >> Timer.isDone env.clock)


viewExplosion : Env -> Explosion -> Shape
viewExplosion env { position, timer, shape } =
    let
        progress =
            Timer.value env.clock timer
    in
    shape
        |> fade (1 - progress)
        |> scale (1 + progress)
        |> move (Pt.toTuple position)



-- ViewHelpers


viewFilledCircle color radius position =
    circle radius
        |> fill color
        |> move (Pt.toTuple position)



-- World


type alias World =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , explosions : List Explosion
    }


initWorld : List Turret -> World
initWorld turrets =
    { player = initialPlayer
    , turrets = turrets
    , bullets = []
    , explosions = []
    }


stepWorld : Env -> World -> World
stepWorld env game =
    let
        ( turretResponse, turrets ) =
            updateTurrets env game game.turrets

        ( bulletResponse, bullets ) =
            updateBullets env game game.bullets
    in
    { game
        | player = updatePlayer env game.player
        , turrets = turrets
        , bullets = bullets ++ turretResponse.bullets
        , explosions =
            updateExplosions env game.explosions
                ++ bulletResponse.explosions
    }


viewWorld : Env -> World -> Shape
viewWorld env game =
    group
        [ viewPlayer game.player
        , List.map viewTurret game.turrets
            |> group
        , List.map viewBullet game.bullets
            |> group
        , List.map (viewExplosion env) game.explosions
            |> group
        ]



-- Game


turretPositions : List Point
turretPositions =
    let
        dst =
            150
    in
    [ ( -1, -1 ), ( 1, 1 ), ( 1, -1 ), ( -1, 1 ) ]
        |> List.map (Tuple.mapBoth ((*) dst) ((*) dst) >> Pt.xy)


turretsForLevel : Float -> Int -> List Turret
turretsForLevel clock level_ =
    let
        level =
            modBy 4 level_

        clockForIdx i =
            clock - (60 / toFloat (level + 1) * toFloat (level - i))
    in
    List.take (level + 1) turretPositions
        --|> List.map (initTurret clock)
        |> List.indexedMap (\i -> initTurret (clockForIdx i))


type alias Game =
    { world : World
    , level : Int
    }


initialGame : Game
initialGame =
    let
        level =
            3
    in
    { world = initWorld (turretsForLevel 0 level)
    , level = level
    }


updateGame : Env -> Game -> Game
updateGame env game =
    let
        world =
            stepWorld env game.world

        isLevelComplete =
            world.turrets |> List.isEmpty
    in
    if isLevelComplete then
        let
            nextLevel =
                game.level + 1
        in
        { game
            | world = { world | turrets = turretsForLevel env.clock nextLevel }
            , level = nextLevel
        }

    else
        { game | world = world }


viewGame : Env -> Game -> Shape
viewGame env game =
    group
        [ viewWorld env game.world
        ]



--


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view model =
    let
        env =
            toEnv model
    in
    renderShapes env.screen
        [ viewGame env model.game ]


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


toEnv : Model -> Env
toEnv model =
    { mousePosition = model.mousePosition
    , screen = model.screen
    , clock = model.clock
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick _ ->
            let
                env : Env
                env =
                    toEnv model
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
