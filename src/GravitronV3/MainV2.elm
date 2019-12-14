module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody exposing (CircularBody, RigidBody)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import PointFree as PF exposing (rejectWhen)
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


playerToShape : Player -> Shape
playerToShape player =
    circle player.radius
        |> fill "green"



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


updateBullets : Env -> BulletCtx bc -> List Bullet -> List Response
updateBullets env ctx =
    let
        reducer :
            ( Bullet, List Bullet )
            -> Response
        reducer ( bullet, otherBullets ) =
            if isBulletIntersecting ctx otherBullets bullet then
                AddExplosion (explosionFrom env bulletToShape bullet)

            else
                AddBullet (updateBullet env ctx bullet)
    in
    List.Extra.select >> List.map reducer


bulletToShape : Bullet -> Shape
bulletToShape { radius } =
    group
        [ circle radius
            |> fill "black"
            |> fade 0.7
        ]



-- Turret


type alias TurretPlaceholder =
    { counter : Counter
    , position : Point
    , turret : Turret
    }


initTurretPlaceholder : Float -> Turret -> TurretPlaceholder
initTurretPlaceholder delay turret =
    TurretPlaceholder (Counter.initDelayedBy delay 60) turret.position turret


turretPlaceHolderToShape : TurretPlaceholder -> Shape
turretPlaceHolderToShape { counter, turret } =
    let
        progress =
            Counter.progress counter
    in
    turretToShape turret
        |> fade progress
        |> scale progress


updateTurretPlaceHolder : TurretPlaceholder -> Response
updateTurretPlaceHolder model =
    let
        ( isDone, counter ) =
            Counter.step model.counter
    in
    if isDone then
        AddTurret model.turret

    else
        AddTurretPlaceholder { model | counter = counter }


updateTurretPlaceHolders : List TurretPlaceholder -> List Response
updateTurretPlaceHolders =
    List.map updateTurretPlaceHolder


type alias HP =
    { current : Int, max : Int }


type alias Turret =
    { position : Point
    , velocity : Vec
    , radius : Float
    , bulletTimer : Counter
    , hp : HP
    }


initHP : Int -> HP
initHP maxHP =
    HP maxHP maxHP


initTurret : Point -> Turret
initTurret point =
    { position = point
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Counter.init 60
    , hp = initHP 2
    }


type alias TurretCtx tc =
    { tc
        | player : Player
        , bullets : List Bullet
    }


isTurretIntersecting : TurretCtx tc -> Turret -> Bool
isTurretIntersecting ctx turret =
    List.any (RigidBody.doCircleOverlap turret) ctx.bullets



-- || RigidBody.doCircleOverlap turret ctx.player


type alias HasHP a =
    { a | hp : HP }


isDead : HasHP a -> Bool
isDead { hp } =
    hp.current <= 0


hit : HasHP a -> HasHP a
hit hasHP =
    let
        decCurrentHP hp =
            { hp | current = max 0 (hp.current - 1) }
    in
    { hasHP | hp = decCurrentHP hasHP.hp }


hpPct : HasHP a -> Float
hpPct { hp } =
    toFloat hp.current / toFloat hp.max


when =
    PF.when


updateTurret : Env -> TurretCtx tc -> Turret -> Response
updateTurret env ctx turret =
    if isDead turret then
        AddExplosion (explosionFrom env turretToShape turret)

    else
        let
            ( isDone, bulletTimer ) =
                Counter.step turret.bulletTimer

            addTurretResponse =
                { turret | bulletTimer = bulletTimer }
                    |> when (isTurretIntersecting ctx) hit
                    |> AddTurret
        in
        if isDone then
            Batch
                [ AddBullet (initBullet |> setPosVelFromTo turret ctx.player)
                , addTurretResponse
                ]

        else
            addTurretResponse


updateTurrets : Env -> TurretCtx tc -> List Turret -> List Response
updateTurrets env ctx =
    List.map (updateTurret env ctx)


turretToShape : Turret -> Shape
turretToShape { radius } =
    group
        [ circle radius
            |> fill "red"
            |> fade 0.7
        ]



-- Explosion


type alias Explosion =
    { position : Point
    , shape : Shape
    , timer : Timer
    }


explosionFrom :
    Env
    -> ({ a | position : Point } -> Shape)
    -> { a | position : Point }
    -> Explosion
explosionFrom env func entity =
    { position = entity.position
    , shape = func entity
    , timer = Timer.start env.clock 60
    }


updateExplosions : Env -> List Explosion -> List Explosion
updateExplosions env =
    rejectWhen (.timer >> Timer.isDone env.clock)


explosionToShape : Env -> Explosion -> Shape
explosionToShape env { position, timer, shape } =
    let
        progress =
            Timer.value env.clock timer
    in
    shape
        |> fade (1 - progress)
        |> scale (1 + (progress / 2))



-- World


type alias World =
    { player : Player
    , turretPlaceholders : List TurretPlaceholder
    , turrets : List Turret
    , bullets : List Bullet
    , explosions : List Explosion
    }


initWorld : List TurretPlaceholder -> World
initWorld turretPlaceholders =
    { player = initialPlayer
    , turretPlaceholders = turretPlaceholders
    , turrets = []
    , bullets = []
    , explosions = []
    }


type Response
    = AddExplosion Explosion
    | AddBullet Bullet
    | AddTurret Turret
    | AddTurretPlaceholder TurretPlaceholder
    | Batch (List Response)


flattenResponse : Response -> List Response -> List Response
flattenResponse response acc =
    case response of
        Batch list ->
            List.foldl flattenResponse acc list

        _ ->
            response :: acc


foldResponse : Response -> World -> World
foldResponse response world =
    case response of
        AddExplosion explosion ->
            { world | explosions = explosion :: world.explosions }

        AddBullet bullet ->
            { world | bullets = bullet :: world.bullets }

        AddTurret turret ->
            { world | turrets = turret :: world.turrets }

        Batch responses ->
            foldResponses responses world

        AddTurretPlaceholder turretPlaceHolder ->
            { world
                | turretPlaceholders =
                    turretPlaceHolder :: world.turretPlaceholders
            }


foldResponses : List Response -> World -> World
foldResponses responses world =
    List.foldl foldResponse world responses


updateWorld : Env -> World -> World
updateWorld env world =
    { world
        | player = updatePlayer env world.player
        , turretPlaceholders = []
        , turrets = []
        , bullets = []
        , explosions = updateExplosions env world.explosions
    }
        |> foldResponses (updateTurrets env world world.turrets)
        |> foldResponses (updateBullets env world world.bullets)
        |> foldResponses (updateTurretPlaceHolders world.turretPlaceholders)


viewWorld : Env -> World -> Shape
viewWorld env game =
    group
        [ viewAllHelp turretPlaceHolderToShape game.turretPlaceholders
            |> group
        , viewAllHelp turretToShape game.turrets
            |> group
        , viewHelp playerToShape game.player
        , viewAllHelp bulletToShape game.bullets
            |> group
        , viewAllHelp (explosionToShape env) game.explosions
            |> group
        ]



-- View Helpers


viewHelp toShapeFunc m =
    toShapeFunc m
        |> Canvas.move (Pt.toTuple m.position)


viewAllHelp toShapeFunc =
    List.map (viewHelp toShapeFunc)



--noinspection ElmUnusedSymbol


move =
    never



-- Game


turretPositions : List Point
turretPositions =
    let
        dst =
            150
    in
    [ ( -1, -1 ), ( 1, 1 ), ( 1, -1 ), ( -1, 1 ) ]
        |> List.map (Tuple.mapBoth ((*) dst) ((*) dst) >> Pt.xy)


turretPlaceholdersForLevel : Int -> List TurretPlaceholder
turretPlaceholdersForLevel level_ =
    let
        level =
            modBy 4 level_
    in
    List.take (level + 1) turretPositions
        |> List.map initTurret
        |> List.indexedMap
            (\i ->
                initTurretPlaceholder
                    (toFloat i / toFloat (level + 1))
            )


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
    { world = initWorld (turretPlaceholdersForLevel level)
    , level = level
    }


updateGame : Env -> Game -> Game
updateGame env game =
    let
        world =
            updateWorld env game.world

        isLevelComplete =
            List.isEmpty world.turrets && List.isEmpty world.turretPlaceholders
    in
    if isLevelComplete then
        let
            nextLevel =
                game.level + 1
        in
        { game
            | world = { world | turretPlaceholders = turretPlaceholdersForLevel nextLevel }
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
