module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody exposing (Circular, CircularBody, RigidBody)
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


initBullet : Circular a -> Float -> Bullet
initBullet gun angle =
    let
        radius =
            10

        speed =
            3
    in
    { position =
        Pt.moveBy
            (Vec.fromRTheta (radius + gun.radius) angle)
            gun.position
    , velocity = Vec.fromRTheta speed angle
    , radius = radius
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
    , kind : TurretKind
    }


initHP : Int -> HP
initHP maxHP =
    HP maxHP maxHP


initTurret : ( Point, TurretKind ) -> Turret
initTurret ( point, kind ) =
    let
        maxHP =
            case kind of
                GravityShooter1 ->
                    1

                GravityShooter2 ->
                    2

                TripleGravityShooter ->
                    3

                GravityShooterOnDeathShoot5 ->
                    2
    in
    { position = point
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Counter.init 60
    , hp = initHP maxHP
    , kind = kind
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


hpPct : HP -> Float
hpPct hp =
    toFloat hp.current / toFloat hp.max


when =
    PF.when


fireBulletOnTrigger : Player -> Turret -> Response
fireBulletOnTrigger player turret =
    let
        angle =
            Pt.vecFromTo turret.position player.position
                |> Vec.angle
    in
    case turret.kind of
        GravityShooter1 ->
            AddBullet (initBullet turret angle)

        GravityShooter2 ->
            AddBullet (initBullet turret angle)

        TripleGravityShooter ->
            let
                angleSpread =
                    turns (1 / 8)
            in
            [ angle - angleSpread, angle, angle + angleSpread ]
                |> List.map (initBullet turret >> AddBullet)
                |> Batch

        GravityShooterOnDeathShoot5 ->
            AddBullet (initBullet turret angle)


turretDeathResponse : Env -> Turret -> Response
turretDeathResponse env turret =
    let
        addExplosion =
            AddExplosion (explosionFrom env turretToShape turret)
    in
    case turret.kind of
        GravityShooter1 ->
            addExplosion

        GravityShooter2 ->
            addExplosion

        TripleGravityShooter ->
            addExplosion

        GravityShooterOnDeathShoot5 ->
            let
                angleFrac =
                    turns (1 / 5)

                angles =
                    List.range 0 4
                        |> List.map (toFloat >> (*) angleFrac)
            in
            angles
                |> List.map (initBullet turret >> AddBullet)
                |> (::) addExplosion
                |> Batch


updateTurret : Env -> TurretCtx tc -> Turret -> Response
updateTurret env ctx turret =
    if isDead turret then
        turretDeathResponse env turret

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
                [ fireBulletOnTrigger ctx.player turret
                , addTurretResponse
                ]

        else
            addTurretResponse


updateTurrets : Env -> TurretCtx tc -> List Turret -> List Response
updateTurrets env ctx =
    List.map (updateTurret env ctx)


turretToShape : Turret -> Shape
turretToShape { radius, hp, kind } =
    let
        color =
            case kind of
                GravityShooter1 ->
                    "red"

                GravityShooter2 ->
                    "blue"

                TripleGravityShooter ->
                    "green"

                GravityShooterOnDeathShoot5 ->
                    "purple"

        fullShape =
            group
                [ circle radius
                    |> fill color
                    |> fade 0.7
                ]
    in
    group
        [ fullShape |> fade 0.5
        , fullShape |> fade 0.5 |> scale (hpPct hp)
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
viewWorld env world =
    group
        [ viewAllHelp turretPlaceHolderToShape world.turretPlaceholders
            |> group
        , viewAllHelp turretToShape world.turrets
            |> group
        , viewHelp playerToShape world.player
        , viewAllHelp bulletToShape world.bullets
            |> group
        , viewAllHelp (explosionToShape env) world.explosions
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



-- Levels


type alias LevelId =
    ( Int, Int )


type TurretKind
    = GravityShooter1
    | GravityShooter2
    | TripleGravityShooter
    | GravityShooterOnDeathShoot5


type alias LevelConfig =
    List SubLevelConfig


type alias SubLevelConfig =
    List TurretKind


levels : List LevelConfig
levels =
    [ -- level 1
      [ [ GravityShooter1 ]
      , [ GravityShooter1, GravityShooter1 ]
      , [ GravityShooter1, GravityShooter2 ]
      , [ GravityShooter2, GravityShooter2 ]
      , List.repeat 4 GravityShooter2
      ]

    -- test level
    , [ [ GravityShooterOnDeathShoot5
        , GravityShooterOnDeathShoot5
        , TripleGravityShooter
        , TripleGravityShooter
        ]
      , [ GravityShooter1, GravityShooter1 ]
      , [ GravityShooter1, GravityShooter2 ]
      , [ GravityShooter2, GravityShooter2 ]
      , List.repeat 4 GravityShooter2
      ]
    ]


maxLevels : Int
maxLevels =
    List.length levels


nextLevelId : LevelId -> LevelId
nextLevelId ( major, minor ) =
    if minor >= 4 then
        ( major + 1 |> modBy maxLevels, 0 )

    else
        ( major, minor + 1 )


getSubLevelConfig : LevelId -> SubLevelConfig
getSubLevelConfig ( major, minor ) =
    levels
        |> List.drop major
        |> List.head
        |> Maybe.andThen (List.drop minor >> List.head)
        |> Maybe.withDefault []



-- Game


turretPositions : List Point
turretPositions =
    let
        dst =
            150
    in
    [ ( -1, -1 ), ( 1, 1 ), ( 1, -1 ), ( -1, 1 ) ]
        |> List.map (Tuple.mapBoth ((*) dst) ((*) dst) >> Pt.xy)


turretPlaceholdersForLevel : LevelId -> List TurretPlaceholder
turretPlaceholdersForLevel levelId =
    let
        subLevelConfig : SubLevelConfig
        subLevelConfig =
            getSubLevelConfig levelId

        turretCount =
            subLevelConfig |> List.length
    in
    List.map2 Tuple.pair turretPositions subLevelConfig
        |> List.indexedMap
            (\i ->
                initTurret
                    >> initTurretPlaceholder (toFloat i / toFloat turretCount)
            )


type alias Game =
    { world : World
    , levelId : LevelId
    }


initialGame : Game
initialGame =
    let
        level =
            ( 1, 0 )
    in
    { world = initWorld (turretPlaceholdersForLevel level)
    , levelId = level
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
            newLevel =
                nextLevelId game.levelId

            majorChanged =
                Tuple.first game.levelId /= Tuple.first newLevel

            newTurretPlaceholders =
                turretPlaceholdersForLevel newLevel

            newWorld =
                if majorChanged then
                    { world
                        | turretPlaceholders = newTurretPlaceholders
                        , bullets = []
                    }

                else
                    { world
                        | turretPlaceholders = newTurretPlaceholders
                    }
        in
        { game
            | world = newWorld
            , levelId = newLevel
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
                    | mousePosition =
                        vec (pageX + screen.left) (pageY + screen.top)
                }


subscriptions _ =
    Sub.batch
        [ Screen.onResize GotScreen
        , E.onAnimationFrame Tick
        , D.map2 MouseMove
            (D.field "pageX" D.float)
            (D.field "pageY" D.float)
            |> E.onMouseMove
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
