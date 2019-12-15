module GravitronV3.MainV2 exposing (main)

import Basics.Extra exposing (inDegrees)
import Browser
import Browser.Events as E
import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody
    exposing
        ( Circular
        , CircularBody
        , RigidBody
        )
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import PointFree as PF
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


bounceWithinScreen : Screen -> Float -> RigidBody a -> Vec
bounceWithinScreen screen factor m =
    bounceWithinScreenHelp screen
        (m.position |> (Pt.toTuple >> Vec.fromTuple))
        factor
        m.velocity


gravitateTo : Circular target -> RigidBody model -> Vec
gravitateTo target model =
    model.velocity
        |> Vec.add
            (Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (\mag -> 20 / mag)
            )


homingTo : Circular target -> RigidBody model -> Vec
homingTo target model =
    let
        homingVec =
            Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (always 0.3)
    in
    model.velocity
        |> Vec.add homingVec
        |> Vec.mapMagnitude ((*) 0.98)



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


updatePlayer : Screen -> Player -> Player
updatePlayer screen =
    RigidBody.stepWithSeed
        [ randomWalk
        , bounceWithinScreen screen 1 >> Random.constant
        ]


playerToShape : Player -> Shape
playerToShape player =
    circle player.radius
        |> fill "green"



-- Bullet


type BulletKind
    = Gravity
    | Homing
    | GravityTimeBomb Counter


type alias Bullet =
    CircularBody { kind : BulletKind }


initBullet : BulletKind -> Circular a -> Float -> Bullet
initBullet kind gun angle =
    let
        radius =
            6

        speed =
            2
    in
    { position =
        Pt.moveBy
            (Vec.fromRTheta (radius + gun.radius) angle)
            gun.position
    , velocity = Vec.fromRTheta speed angle
    , radius = radius
    , kind = kind
    }


initGravityTimeBombBullet : Circular a -> Float -> Bullet
initGravityTimeBombBullet =
    initBullet (GravityTimeBomb (Counter.init (60 * 3)))


initGravityBullet : Circular a -> Float -> Bullet
initGravityBullet =
    initBullet Gravity


initHomingBullet : Circular a -> Float -> Bullet
initHomingBullet =
    initBullet Homing


isBulletIntersecting : BulletCtx a p t -> List Bullet -> Bullet -> Bool
isBulletIntersecting ctx otherBullets bullet =
    RigidBody.doCircleOverlap bullet ctx.player
        || List.any (RigidBody.doCircleOverlap bullet) ctx.turrets
        || List.any (RigidBody.doCircleOverlap bullet) otherBullets


type alias BulletCtx a p t =
    { a
        | player : Circular p
        , turrets : List (Circular t)
    }


stepBulletCounter : Bullet -> Bullet
stepBulletCounter bullet =
    case bullet.kind of
        Gravity ->
            bullet

        Homing ->
            bullet

        GravityTimeBomb counter ->
            { bullet | kind = GravityTimeBomb (Counter.step counter) }


isBulletDead bullet =
    case bullet.kind of
        Gravity ->
            False

        Homing ->
            False

        GravityTimeBomb counter ->
            Counter.isDone counter


bulletDeathResponse : Bullet -> Response
bulletDeathResponse bullet =
    let
        addBulletExplosion =
            explosionFrom bulletToShape bullet |> AddExplosion
    in
    case bullet.kind of
        Gravity ->
            addBulletExplosion

        Homing ->
            addBulletExplosion

        GravityTimeBomb _ ->
            addBulletExplosion


updateBullets : Screen -> BulletCtx a p t -> List Bullet -> List Response
updateBullets screen ctx =
    let
        update_ : ( Bullet, List Bullet ) -> Response
        update_ ( bullet, otherBullets ) =
            if
                isBulletIntersecting ctx otherBullets bullet
                    || isBulletDead bullet
            then
                explosionFrom bulletToShape bullet
                    |> AddExplosion

            else
                bullet
                    |> RigidBody.step
                        [ case bullet.kind of
                            Gravity ->
                                gravitateTo ctx.player

                            Homing ->
                                homingTo ctx.player

                            GravityTimeBomb _ ->
                                gravitateTo ctx.player
                        , bounceWithinScreen screen 0.5
                        ]
                    |> stepBulletCounter
                    |> AddBullet
    in
    List.Extra.select >> List.map update_


bulletToShape : Bullet -> Shape
bulletToShape bullet =
    let
        { radius, kind, velocity } =
            bullet

        otherShape =
            case kind of
                Gravity ->
                    group []

                Homing ->
                    group
                        [ rect (radius * 3.5) 1
                            |> rotate (Vec.angle velocity |> inDegrees)
                        ]

                GravityTimeBomb _ ->
                    group []
    in
    group
        [ group
            [ circle radius
            , otherShape
            ]
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
            Counter.cycleStep model.counter
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

                HomingShooter ->
                    4

                TimeBombShooter ->
                    3
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
            AddBullet (initGravityBullet turret angle)

        GravityShooter2 ->
            AddBullet (initGravityBullet turret angle)

        TripleGravityShooter ->
            let
                angleSpread =
                    turns (1 / 8)
            in
            [ angle - angleSpread, angle, angle + angleSpread ]
                |> List.map (initGravityBullet turret >> AddBullet)
                |> Batch

        GravityShooterOnDeathShoot5 ->
            AddBullet (initGravityBullet turret angle)

        HomingShooter ->
            AddBullet (initHomingBullet turret angle)

        TimeBombShooter ->
            AddBullet (initGravityTimeBombBullet turret angle)


turretDeathResponse : Turret -> Response
turretDeathResponse turret =
    let
        responseHelp bulletCt =
            addTurretExplosionWithBullets bulletCt turret
    in
    case turret.kind of
        GravityShooter1 ->
            responseHelp 0

        GravityShooter2 ->
            responseHelp 0

        TripleGravityShooter ->
            responseHelp 0

        GravityShooterOnDeathShoot5 ->
            responseHelp 5

        HomingShooter ->
            responseHelp 0

        TimeBombShooter ->
            responseHelp 0


addTurretExplosionWithBullets : Int -> Turret -> Response
addTurretExplosionWithBullets bulletCt turret =
    breakTurn bulletCt
        |> List.map (initGravityBullet turret >> AddBullet)
        |> (::) (AddExplosion (explosionFrom turretToShape turret))
        |> Batch


breakTurn : Int -> List Float
breakTurn parts =
    if parts <= 0 then
        []

    else
        let
            angleFrac =
                turns (1 / toFloat parts)
        in
        List.range 1 parts
            |> List.map ((+) -1 >> toFloat >> (*) angleFrac)


turretStepResponse : TurretCtx ct -> Turret -> Response
turretStepResponse ctx turret =
    let
        ( isDone, bulletTimer ) =
            Counter.cycleStep turret.bulletTimer

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


updateTurret : TurretCtx tc -> Turret -> Response
updateTurret ctx turret =
    if isDead turret then
        turretDeathResponse turret

    else
        turretStepResponse ctx turret


updateTurrets : TurretCtx tc -> List Turret -> List Response
updateTurrets ctx =
    List.map (updateTurret ctx)


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

                HomingShooter ->
                    "orange"

                TimeBombShooter ->
                    "deeppink"

        fullShape =
            group
                [ circle radius
                    |> fill color
                    |> fade 0.7
                ]
    in
    group
        [ fullShape |> fade 0.9
        , fullShape |> scale (hpPct hp)
        ]



-- Explosion


type alias Explosion =
    { position : Point
    , shape : Shape
    , timer : Counter
    }


explosionFrom :
    ({ a | position : Point } -> Shape)
    -> { a | position : Point }
    -> Explosion
explosionFrom func entity =
    { position = entity.position
    , shape = func entity
    , timer = Counter.init 60
    }


updateExplosion : Explosion -> Response
updateExplosion explosion =
    let
        ( done, timer ) =
            Counter.cycleStep explosion.timer
    in
    if done then
        Batch []

    else
        AddExplosion { explosion | timer = timer }


updateExplosions : List Explosion -> List Response
updateExplosions =
    List.map updateExplosion


explosionToShape : Explosion -> Shape
explosionToShape { position, timer, shape } =
    let
        progress =
            Counter.progress timer
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
    let
        { screen } =
            env
    in
    { world
        | player = updatePlayer screen world.player
        , turretPlaceholders = []
        , turrets = []
        , bullets = []
        , explosions = []
    }
        |> foldResponses (updateTurrets world world.turrets)
        |> foldResponses (updateBullets screen world world.bullets)
        |> foldResponses (updateTurretPlaceHolders world.turretPlaceholders)
        |> foldResponses (updateExplosions world.explosions)


viewWorld : World -> Shape
viewWorld world =
    group
        [ viewAllHelp turretPlaceHolderToShape world.turretPlaceholders
            |> group
        , viewAllHelp turretToShape world.turrets
            |> group
        , viewHelp playerToShape world.player
        , viewAllHelp bulletToShape world.bullets
            |> group
        , viewAllHelp explosionToShape world.explosions
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
    | HomingShooter
    | TimeBombShooter


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
    , [ [ TimeBombShooter
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


viewGame : Game -> Shape
viewGame game =
    group
        [ viewWorld game.world
        ]



--


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view model =
    renderShapes model.screen
        [ viewGame model.game ]


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
